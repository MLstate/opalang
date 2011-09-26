#!/usr/bin/env bash

set -u
set -e

#
#   Copyright © 2011 MLstate
#
#   This file is part of OPA.
#
#   OPA is free software: you can redistribute it and/or modify it under the
#   terms of the GNU Affero General Public License, version 3, as published by
#   the Free Software Foundation.
#
#   OPA is distributed in the hope that it will be useful, but WITHOUT ANY
#   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
#   FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
#   more details.
#
#   You should have received a copy of the GNU Affero General Public License
#   along with OPA. If not, see <http://www.gnu.org/licenses/>.
#

#     Author: Louis Gesbert
#     Author: Hugo Venturini

DEBUG=${DEBUG:-0}

echo_debug () {
    if [ ! "$DEBUG" = "0" ]; then
        local msg=""
        local option=""
        case $1 in
            -n)
                shift
                option="-n"
                msg=$2; shift
                ;;
            *)
                msg=$1; shift
                ;;
        esac
        [ $# -eq 0 ]
        echo $option "[35m$msg[0m"
    fi
}


USERS=()
HOSTS=()
PORTS=()
TYPES=()
DB_OPTIONS=()
OPA_SERVER=()
DEFAULT_PORT=8081
HAPROXY_PORT=8080
SSH_ENV="MLSTATE_FORCE_COLOR=1"
OPA_DB_ENABLED="true"

PEM_FILE=""
MDNS=""

if ! AWK=$(which awk); then
    AWK=""
fi

if ! OPA_DB_SERVER=$(which opa-db-server); then
    OPA_DB_SERVER=""
fi

if ! HAPROXY_BIN=$(which haproxy); then
    HAPROXY_BIN=""
fi

OS=$(uname -s)
TMP_DIR=""
SEQ=""



case $OS in
    Darwin)
        export TMPDIR="/tmp"
# for some reason some mac have TMPDIR set to '/var/folder/<something>' which is not compatible with every mac ...
        TMP_DIR=$(mktemp -d -t opa-run-$(hostname)-$$)
        if ! MDNS=$(which dns-sd); then
            echo_debug "'dns-sd' not found, are you using .local addresses?"
            MDNS=""
        else
            MDNS="$MDNS -G v4"
        fi
        SEQ="gseq"
        ;;
    Linux|*)
        # no no no ... it does NOT mean the same thing !
        TMP_DIR=$(mktemp -t -d opa-run-$(hostname)-$$-XXXX)
        if ! MDNS=$(which avahi-resolve); then
            echo_debug "'avahi-resolve' not found, are you using .local addresses?"
            MDNS=""
        else
            MDNS="$MDNS -n"
        fi
        SEQ="seq"
        ;;
esac

HAPROXY_FILE="$TMP_DIR/haproxy.conf"
HAPROXY_PIDS="$TMP_DIR/haproxy.pids"

display_help () {
  cat <<EOF
Usage: $0 <options> <service> [service-options]

Dispatcher for a service across a network: runs <service> on each <host> with
a single local database server, and provides a local load-balancing front-end
to access them via HTTP.

Options:
	--help -h				Displays the current help and exits
	--host <[user@]host[:port]>[,n]		Specifies a host to use for distribution.
						The user will be used for login, your service will be run on
						the given port. n specifies the number of instances you want,
						each running on port 'port+n'.
	--host-pem <[user@]host[:port]>[,n]	Specifies a host to use for distribution with public-key.
						The user will be used for login, your service will be run on
						the given port. n specifies the number of instances you want,
						each running on port 'port+n'. (see '--pem' option)
	--pem <path>				Specify a public-key to connect to host. Will be used with
						hosts declared with '--host-pem' option.
	--opa-port <port>			Use the given http port by default on the
						opa services (default: $DEFAULT_PORT)
	--haproxy <path>			The path to your haproxy binary (default: $HAPROXY_BIN)
	--port <port>				The main port to listen on (default: $HAPROXY_PORT)
	--opa-db-server <path>			Specifies the path to the Opa database server binary
	--no-db					Does not set up any remote database server

EOF
}


parse_and_add_host () {
    [ $# -eq 2 ]
    local input=$1;shift
    local type=$1; shift; [ "$type" = "ec2" ] || [ "$type" = "classic" ]
    [ $# -eq 0 ]

    local hp=${input%,*}
    local count=${input##*,}
    [ "$count" != "$input" ] || count=1

    local HOST=${hp%:*}
    local PORT=${hp##*:}
    [ "$PORT" != "$hp" ] || PORT=$DEFAULT_PORT

    local L_USER=${HOST%@*}
    local L_HOST=${HOST##*@}
    [ "$L_HOST" != "$HOST" ] || L_USER=$USER
    for i in $($SEQ 0 $((count-1))) ; do
        echo_debug "Adding $L_USER@$L_HOST:$(($PORT+$i)) as $type instance."
        USERS[${#USERS[@]}]=$(echo "$L_USER" | tr '[:upper:]' '[:lower:]')
        HOSTS[${#HOSTS[@]}]=$(echo "$L_HOST" | tr '[:upper:]' '[:lower:]')
        PORTS[${#PORTS[@]}]=$(($PORT+$i))
        TYPES[${#TYPES[@]}]=$(echo $type | tr '[:upper:]' '[:lower:]')
    done
}

while [ $# -gt 0 ]; do
    case $1 in
        --help|-h)
            display_help
            exit 1
            ;;
        --host)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi; shift
            parse_and_add_host $1 "classic"
            ;;
        --host-pem)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi; shift
            parse_and_add_host $1 "ec2"
            ;;
        --hostfile)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi; shift
            H=($(<$1))
            for h in "${H[@]}"; do parse_and_add_host "$h" "classic"; done
            ;;
        --opa-port)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi; shift
            DEFAULT_PORT=$1
            ;;
        --port)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi; shift
            HAPROXY_PORT=$1
            ;;
        --pem)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi; shift
            PEM_FILE=$1
            ;;
        --no-db)
            OPA_DB_ENABLED="false"
            ;;
        --opa-db-server)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi; shift
            OPA_DB_SERVER=$1
            ;;
        --db-*)
            DB_OPTIONS[${#DB_OPTIONS[@]}]=$1
            if [ $# -ge 2 ] && [ "${2#-}" = "$2" ]; then
                shift
                DB_OPTIONS[${#DB_OPTIONS[@]}]=$1
            fi
            ;;
        --haproxy)
            if [ $# -lt 2 ]; then echo "Error: option $1 requires an argument"; exit 1; fi; shift
            HAPROXY_BIN="$1";
            ;;
        --*|-*)
            echo "Unknown option '$1'"
            exit 1
            ;;
        *)
            OPA_SERVER[${#OPA_SERVER[@]}]=$1
    esac
    shift
done


##
## check parameters and make suggestion if incomplete
##
suggest_help () {
    local error=$1
    local suggest=$2
    echo "[31m$error[0m"
    display_help
    echo "[36m$suggest[0m"
    exit 1
}
if [ "$OPA_DB_SERVER" = "" ] && [ "$OPA_DB_ENABLED" = "true" ] ; then
    suggest_help "You must have 'opa-db-server' in your path." "Option '--no-db' may be an alternative."
fi
if [ ! -x "$OPA_DB_SERVER" ] && [ "$OPA_DB_ENABLED" = "true" ] ; then
    suggest_help "'$OPA_DB_SERVER' does not seem to exist or to be executable." "Check '$OPA_DB_SERVER'"
fi
if [ "$HAPROXY_BIN" = "" ] || [ ! -x "$HAPROXY_BIN" ] ; then
    suggest_help "No path or wrong path to HAProxy defined: load-balancing impossible." "Option '--haproxy' may be the solution."
fi
if [ "${#PORTS[@]}" = "0" ] ; then
    suggest_help "You must define at least one host." "Option '--host' may be the solution."
fi
if [ "${#OPA_SERVER[@]}" = "0" ] || [ ! -x "$OPA_SERVER" ] ; then
    suggest_help "You must specify a server to distribute." ""
fi
if [ "$PEM_FILE" != "" ] && [ ! -e "$PEM_FILE" ] ; then
    suggest_help "You must specify a valid .pem file." ""
fi
##
## Actual distribution
##

hostids=$($SEQ 0 $(( ${#PORTS[@]} - 1 )))
for i in $hostids; do
    # if the port was not specified, we pick a supposedly fresh one.
    PORTS[i]=${PORTS[i]/default/$(($DEFAULT_PORT+$i))}
done

echo "Will attempt to run $(basename ${OPA_SERVER[0]}) across ${#HOSTS[@]} remote servers"

echo_debug "Temp files in $TMP_DIR"

DBPORT=$((9200+$$%100))
if [ $OPA_DB_ENABLED = "true" ] ; then
    echo "Running database server (locally)"
    run_server () {
        local ret=0
        case $OS in
            Darwin)
                echo_debug "> $OPA_DB_SERVER -p $DBPORT ${DB_OPTIONS[@]:-} & "
                "$OPA_DB_SERVER" -p $DBPORT ${DB_OPTIONS[@]:-} &
                ;;
            *)
                echo_debug "> $OPA_DB_SERVER -p $DBPORT --daemon $TMP_DIR/db.pid ${DB_OPTIONS[@]:-}"
                "$OPA_DB_SERVER" -p $DBPORT --daemon $TMP_DIR/db.pid ${DB_OPTIONS[@]:-} || ret=$?
                ;;
        esac
        case $ret in
            0)
                echo "Database server up and running" ;;
            21)
                echo "Port $DBPORT seems to be in use, trying again on a different one"
                DBPORT=$((DBPORT+1))
                run_server
                ;;
            *)
                echo "Could not start database server locally, aborting"
                exit 1
        esac
    }
    run_server
else
    echo "[33mNo database server to run[0m"
fi

echo "Initialising OPA servers"

fifos=()

on_host () (
    id=$1
    shift
    if [ ! "$DEBUG" = "0" ] ; then echo "echo \"[35m+ \"'${@//%hostid%/$id}'\"[0m\"" >> ${fifos[id]}; fi
    echo "${@//%hostid%/$id}" >> ${fifos[id]}
)

on_all () (
    for i in $hostids; do
        on_host $i "$@"
    done
)

close_all () {
    echo "[32m== Exiting ==[0m"
    set +e
    echo "Stopping load-balancer..."
    kill -TERM $(<$HAPROXY_PIDS)
    echo Closing connections...
    on_all "kill -TERM \$(cat $TMP_DIR.%hostid%/server.pid)"
    on_all rm -rf /tmp/${TMP_DIR#/tmp/}.%hostid%
    on_all 'exit'
    exec >&4- &
    kill $(jobs -p) 2> /dev/null
    if [ $OPA_DB_ENABLED = "true" ] ; then
        echo Closing database...
        case $OS in
            Darwin) # nothing to do, 'daemon' mode is not compatible with 'kqueue' used on MacOS
                ;;
            *)
                kill $(cat $TMP_DIR/db.pid) 2>/dev/null
                ;;
        esac
    fi
    echo Cleaning up...
    rm -rf /tmp/${TMP_DIR#/tmp/}
}
trap close_all EXIT

echo_debug "Now opening the ssh connections"
for i in $hostids; do
    echo "Connecting to '${HOSTS[i]}' as '${USERS[i]}'"
    FIFO=$TMP_DIR/$i-${USERS[i]}-${HOSTS[i]}.pipe
    mkfifo $FIFO
    debugh=$(printf "[31m%d[0m-[%dm%-10s[0m" $i $(($i+31)) "${HOSTS[i]}:")
    OPTION_PEM=""
    if [ "$PEM_FILE" != "" ] && [ ${TYPES[i]} = "ec2" ] ; then
        OPTION_PEM="-i $PEM_FILE"
    fi
    echo_debug "ssh $OPTION_PEM -e none -C ${USERS[i]}@${HOSTS[i]} 'sh +i'"
    ssh $OPTION_PEM -e none -C ${USERS[i]}@${HOSTS[i]} 'sh +i' <$FIFO 2>&1 | tee $TMP_DIR/$i-${HOSTS[i]}.log | sed "s/^/$debugh /" &
    exec 4>$FIFO #open the pipe for writing
    fifos[i]=$FIFO
done

copy_host () (
    id=$1; src=$2; dst=$3
    echo "echo \"[036mUploading $src[0m\"" >> ${fifos[id]}
    hostname=$(echo "$(uname -n)" | tr '[:upper:]' '[:lower:]')
    if [ ! "$DEBUG" = "0" ] ; then echo "echo \"[035m= Copying $src -> $dst on $hostname[0m\"" >> ${fifos[id]}; fi
    if [ ${HOSTS[id]} == "localhost" ] || [ ${HOSTS[id]} == "127.0.0.1" ] || [ ${HOSTS[id]} == $hostname ] || [ ${HOSTS[id]} == $hostname".local" ] ; then
        # In case it hasn't been done yet, we create the tmp dir.
        mkdir -p $(dirname $dst)
        cp $src "$dst"
    else
        echo "base64 --decode  <<EOF >$dst" >> ${fifos[id]}
        base64 <$src >> ${fifos[id]}
        echo -e "EOF" >> ${fifos[id]}
    fi
)

on_all export $SSH_ENV

on_all mkdir -p $TMP_DIR.%hostid%

for i in $hostids; do
    copy_host $i ${OPA_SERVER[0]} $TMP_DIR.$i/server
done
on_all chmod u+x $TMP_DIR.%hostid%/server

echo "Running OPA servers"
MAIN_HOST=""
{
    hp=${HOSTS[0]}
    TRAILING_DOMAIN=${hp##*.}
    echo_debug "... $hp has domain '$TRAILING_DOMAIN'"
    if [ $TRAILING_DOMAIN = "local" ] ; then
        FILE=$(mktemp $TMP_DIR/dns-sd-XXXXX)
        echo_debug "... Creating temporary file: '$FILE'"
        case $OS in
            Darwin)
                $MDNS $hp | $AWK 'NR != 1 { print $6 > "'"$FILE"'" }'  &
                PID=$!
                sleep 1
                disown $PID
                kill $PID
                MAIN_HOST=$(cat $FILE)
                ;;
            *)
                MAIN_HOST=$($MDNS $hp | $AWK '{ print $2 }')
        esac
        echo_debug "... main host found: '$MAIN_HOST'"
        echo_debug "... Removing temporary file: '$FILE'"
        rm -fr $FILE
    else
        MAIN_HOST=$(nslookup ${HOSTS[0]} | tail -n 2 | grep Address | sed 's/Address: //')
    fi
}

for i in $hostids; do
    OPTIONS="--cloud --hlnet-port $((1086+$i))"
    DB_OPTIONS=""
    if [ $OPA_DB_ENABLED = "true" ] ; then
        DB_OPTIONS="--db-remote $(hostname):$DBPORT"
    else
        DB_OPTIONS="--db-local $TMP_DIR.%hostid%"
    fi
    if [ "$i" -eq "0" ] ; then
        OPTIONS="$OPTIONS --chan-directory own"
        if [ "${#HOSTS[@]}" -eq "1" ] ; then
            OPTIONS="" # sorry about that.
        fi
    else
        OPTIONS="$OPTIONS --chan-directory $MAIN_HOST:1086"
    fi
    on_host $i "$TMP_DIR.%hostid%/server $DB_OPTIONS "${OPA_SERVER[@]:1}" $OPTIONS --pidfile $TMP_DIR.%hostid%/server.pid --opa-server-port ${PORTS[i]} &"
done

echo "[32m== All launched ==[0m"

###
echo "[32m== Launching load balancer ==[0m"
generate_haproxy_config_file() {
    OPTIONS_DEFAULT=""
    if [ "$($HAPROXY_BIN -v | grep version | sed 's/.*version \([0-9].[0-9]\).*/\1/')" = "1.4" ] ; then
        OPTIONS_DEFAULT="option http-server-close"
    fi
    cat >$HAPROXY_FILE <<EOF
global
    maxconn 384
    daemon
    spread-checks 3

defaults
    mode http
    $OPTIONS_DEFAULT
    timeout connect 50000ms
    timeout client 50000ms
    timeout server 50000ms

frontend http-in-1
    bind *:$HAPROXY_PORT
    default_backend b1

backend b1
    appsession ic len 32 timeout 3h
EOF
    for i in $hostids; do
        elt="${HOSTS[i]}:${PORTS[i]}"
        echo "    server server$((i+1)) $elt check"
    done >> $HAPROXY_FILE
    echo "Configuration file for $($HAPROXY_BIN -version)"
    echo_debug "HAProxy  configuration file: '$HAPROXY_FILE'"
}
generate_haproxy_config_file;
if [ -n "$HAPROXY_BIN" ]; then
    "$HAPROXY_BIN" -f "$HAPROXY_FILE" -p "$HAPROXY_PIDS" &
    echo "[32m=="
    echo "[32m==[0m Load balancer launched ---> [32mhttp://$(hostname):$HAPROXY_PORT[0m"
    echo "[32m=="
else
    echo -e "[32m\t FAILED, haproxy is undefined [0m"
fi

wait


## did you really read the entire script?
##
#  irc:     #opalang
#  twitter: @monsieurv


/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <miniupnpc/miniwget.h>
#include <miniupnpc/miniupnpc.h>
#include <miniupnpc/upnpcommands.h>

static struct UPNPUrls urls;
static struct IGDdatas data;

int init_upnp (void)
{
    struct UPNPDev * devlist;
    struct UPNPDev * dev;
    char * descXML;
    int descXMLsize = 0;
    int ipv6 = 0;
    int error = 0;
    //printf("TB : init_upnp()\n");
    memset(&urls, 0, sizeof(struct UPNPUrls));
    memset(&data, 0, sizeof(struct IGDdatas));
    devlist = upnpDiscover(4000, NULL, NULL, 0, ipv6, &error);
    if (devlist) {
	    dev = devlist;
	    while (dev)
		{
                    //printf("DEV %s\n",dev->st);
                    dev = dev->pNext;
		}
	    dev = devlist;

	    while (dev)
		{
		    if (strstr (dev->st, "InternetGatewayDevice"))
			break;
		    dev = dev->pNext;
		}
	    if (!dev)
		dev = devlist; /* defaulting to first device */
	    /*
	    printf("UPnP device :\n"
		   " desc: %s\n st: %s\n",
		   dev->descURL, dev->st);
	    */
	    descXML = miniwget(dev->descURL, &descXMLsize);
	    if (descXML) {
		parserootdesc (descXML, descXMLsize, &data);
		free (descXML); descXML = 0;
		GetUPNPUrls (&urls, &data, dev->descURL);
	    } else {
		return -2;
	    }
	    freeUPNPDevlist(devlist);
	    return 0 ;
    } else {
	return -1;
    }
}

int open_a_port(const char * addr, int extPort, int inPort, const char * proto) {

    int init_res = init_upnp();
    if (init_res != 0) {
	//printf("Upnp init failure : %d\n", init_res);
	return -101 ;
    }

    int portA = extPort;
    int portB = inPort;
    char port_strA[16];
    char port_strB[16];
    int r;
    // printf("TB : upnp_add_redir (%s, %d)\n", addr, portA);
    if (urls.controlURL != NULL) {
	if(urls.controlURL[0] == '\0')
	    {
		//printf("TB : the init was not done !\n");
		return -1;
	    }
    } else {
	return -100;
    }

    sprintf(port_strA, "%d", portA);
    sprintf(port_strB, "%d", portB);
    //printf(":::: %s; %s; %s; %s\n",urls.controlURL,data.first.servicetype,port_strA,addr);

    r = UPNP_AddPortMapping(urls.controlURL, data.first.servicetype,
                            port_strA, port_strB, addr, NULL, proto, NULL, "0");
    /*
    if(r==0) printf("AddPortMapping(%s, %s, %s) success\n", port_strA, port_strB, addr);
    else     printf("AddPortMapping(%s, %s, %s) failed %d\n", port_strA, port_strB, addr,r);
    */
    return r;
}


int close_a_port(const char * remoteHost, const char * extPort, const char * proto) {
    int init_res ;

    if(!proto || !extPort) {
	return 1;
    }

    init_res = init_upnp();
    if (init_res != 0) {
	//printf("Upnp init failure : %d\n", init_res);
	return -101 ;
    }

    int r = UPNP_DeletePortMapping(urls.controlURL, data.first.servicetype, extPort, proto, 0);
    /*
    printf("--------- {C} close_a_port: remoteHost='%s'; extPort='%s'; proto='%s'; \t r='%d'\n",
	   remoteHost, extPort, proto, r) ;
    */
    return r;
}

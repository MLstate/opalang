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
/**
 * Librarie for functionnal graphs.
 *
 * @author Mathieu Barbin
 * @category algorithmic
 * @destination public
 * @stabilization Totally untested, beware by using this code
 */


/**
 * {1 About this module}
 *
 * This module contains a high-level interface for manipulating functionnal graphs.
 * The kind of graph is
 * + directed
 * + vertex-labeled
 * + edge-labeled
 *
 * It is inspired and adapted from OcamlGraph Signatures and Persitent implementation.
 *
 * This representation is adapted for several cases, from simple directed graphs to automatons.
 */

/**
 * {1 Types defined in this module}
 */

/**
 * The type for indexing the nodes of a graph.
 * We use the native type [int] as uniq identifier. Creating a new node
 * will use a call to a fresh maker, which is on the server. If a client uses
 * this lib, there will be some request to the server for generating nodes.
 */
type FGraph.index = int
type FGraph.index.order = Order.default

/**
 * The type for a vertex. A vertex is indexed with an uniq index, generated
 * via a fresh maker.
 * The label is an info you can attach to a vertex. Different vertices can
 * share the same label, comparison between vertex is always computed only
 * from the index. You cannot change the label once the vertex is created.
 *
 * <!> The type should be private, and not manipulated by user of the FGraph lib.
 * (currently not possible in opa). Please do not use the implementation in your code.
 */
type FGraph.vertex('vertex_label) = {
     index : FGraph.index ;
     label : 'vertex_label ;
}

type FGraph.VertexMap.t('vertex_label, 'elem) = ordered_map(FGraph.vertex('vertex_label), 'elem, FGraph.index.order)
// type FGraph.IndexSet.t('vertex_label) = ordered_set(FGraph.index, FGraph.index.order)

/**
 * The type for an edge. <!> Beware, the label is important for identifying edges.
 * 2 edges between 2 vertices with the same label are in fact the same edge (e.g. automaton)
 * In a graph, you may have several edges between vertices with different labels.
 *
 * <!> The type should be private, and not manipulated by user of the FGraph lib.
 * (currently not possible in opa). Please do not use the implementation in your code.
 */
type FGraph.edge('edge_label, 'vertex_label) = {
     src : FGraph.vertex('vertex_label) ;
     label : 'edge_label ;
     dst : FGraph.vertex('vertex_label) ;
}

/**
 * The type of a node.
 * In this implementation, we have a support for explicitly maintaining edge sets
 * of predecessors. Crucial for algorithms that do a lot of backwards traversal.
 *
 * Some assertion about a [node] pointed in [graph.vertices] by a [vertex] :
 *
 * + all [dst] fields of edges in [in] are physically equal to [vertex]
 * + all [src] fields of edges in [out] are physically equal to [vertex]
 * + in [in] and [out], 2 edges in the list have necessary not the same label,
 *   wrt the [compare_label] function given as argument of the functor FGraph.
 * + the list of edges are ordered wrt the [compare_label] function given as argument in X
 *
 * <!> The type should be private, and not manipulated by user of the FGraph lib.
 * (currently not possible in opa). Please do not use the implementation in your code.
 *
 */
type FGraph.node('edge_label, 'vertex_label) = {
     in : FGraph.VertexMap.t('vertex_label, list(FGraph.edge('edge_label, 'vertex_label))) ;
     out : FGraph.VertexMap.t('vertex_label, list(FGraph.edge('edge_label, 'vertex_label))) ;
}

/**
 * The type of a fgraph.
 * The size is the size of the vertices map, cached just for optimization.
 *
 * <!> The type should be private, and not manipulated by user of the FGraph lib.
 * (currently not possible in opa). Please do not use the implementation in your code.
 */
type FGraph.t('edge_label, 'vertex_label) = {
     vertices : FGraph.VertexMap.t('vertex_label, FGraph.node('edge_label, 'vertex_label)) ;
     size : int ;
}

/**
 * The argument to build a instance of module FGraph.
 * A FGraph module is specialized by :
 * + the type of the label of the edges,
 * + the type of the label of the vertex,
 *
 * The string_of functions are used for producing dot.
 *
 * FIXME: this should be a module interface, not a record interface.
 * Update when this feature will be integrated to S3
 */
type FGraph.X.interface('edge_label, 'vertex_label, 'ordering_edge, 'ordering_vertex) = {
     order_edge_label : order('edge_label, 'ordering_edge);
     order_vertex_label : order('vertex_label, 'ordering_vertex);

     string_of_edge_label : 'edge_label -> string ;
     string_of_vertex_label : 'vertex_label -> string ;
}

/**
 * The functor for creating a new instance of FGraph, depending on the type of
 * the label of the edges. For non labeled edges or vertex, you can use the type [void]
 *
 * FIXME: Define clean interfaces for this module and sub module.
 */
FGraph(X : FGraph.X.interface('edge_label, 'vertex_label, 'ordering_edge, 'ordering_vertex)) = {{

  /**
   * {1 Manipulation of vertices}
   */
  Vertex = {{

    /**
     * {1 Vertices are Comparable}
     */

    ordering(v1: FGraph.vertex, v2: FGraph.vertex)= Int.ordering(v1.index, v2.index)
    compare(v1, v2) = @toplevel.compare((v1 : FGraph.vertex).index, (v2 : FGraph.vertex).index)
    hash(v) = (v : FGraph.vertex).index
    equal(v1, v2) = (v1 : FGraph.vertex).index == (v2 : FGraph.vertex).index

    /**
     * Comparaison based on label
     */
    ordering_label(v1: FGraph.vertex, v2: FGraph.vertex)= Order.ordering(v1.label, v2.label, X.order_vertex_label)
    compare_label(v1, v2) = @opensums(ordering_label(v1, v2))
    /* Type constraint added to prevent value restriction from rejecting. */
    order_label = (Order.make(ordering_label) :
                     order(FGraph.vertex, 'vertex_label))

    /**
     * {1 Vertices are labeled}
     */

    /**
     * The private fresh make used by the FGraph librarie.
    **/
    @private cpt_vertex = Fresh.server(i->i)

    /**
     * Create a new vertex with a given label. This function returns a new node each time it is called.
     * 2 different vertex can share the same label, there will be still different.

     */
    create(label) =
      index = cpt_vertex()
      ~{ index label }

    label(v1) = (v1 : FGraph.vertex).label

  }}

  VertexOrder = Order.make(Vertex.ordering) : order(FGraph.vertex('vertex_label), FGraph.index.order)
  VertexMap = Map_make(VertexOrder)
// VertexSet = Set_make(VertexOrder)

  /**
   * {1 Manipulation of edges}
   */
  Edge = {{

    /**
     * {1 Edges are Ordered}
     */

    /**
     * The comparaison is down in lexicographic order
     * wrt the order :(src, dst, label). Comparaison on vertex is based on uniq index,
     * and comparaison on label is based on the [compare_label] given in argument [X].
     */
    ordering(e1, e2) =
      { src=src1 label=label1 dst=dst1 } = e1
      { src=src2 label=label2 dst=dst2 } = e2
      match Vertex.ordering(src1, src2) with
         | {eq} ->
           match Vertex.ordering(dst1, dst2) with
             | {eq} -> Order.ordering(label1, label2, X.order_edge_label)
             |  r   -> r
           end
         | r    -> r
       end

    compare(e1, e2) = @opensums(ordering(e1, e2))
    /* Type constraint added to prevent value restriction from rejecting. */
    order = (Order.make(ordering):
              order({ dst : FGraph.vertex('vertex_label) ;
                      label : 'edge_label ;
                      src : FGraph.vertex('vertex_label) },
                    'edge_label))

    /**
     * Comparaison based on label
     */
    ordering_label(e1, e2) = Order.ordering((e1 : FGraph.edge).label, (e2 : FGraph.edge).label, X.order_edge_label)
    compare_label(e1, e2) = @opensums(ordering_label(e1, e2))
    /* Type constraint added to prevent value restriction from rejecting. */
    order_label = (Order.make(ordering_label):
                    order(FGraph.edge('edge_label, 'vertex_label), 'edge_label))

    /**
     * {1 Edges are directed}
     */

    src(e) = (e : FGraph.edge('edge_label, 'vertex_label)).src
    dst(e) = (e : FGraph.edge('edge_label, 'vertex_label)).dst

    /**
     * {1 Edges are labeled}
     */

    /**
     * Creating a new edge from its [src], [dst] vertices and its [label].
     * If you create 2 times the same edge, you'll get 2 different objects in memory,
     * but there would be equal wrt the [Edge.compare] function, which assure the good
     * behavior of other primitives manipulating edges.
     */
    create(src, label, dst) = ~{ src label dst} : FGraph.edge('edge_label, 'vertex_label)

    label(edge) = (edge : FGraph.edge('edge_label, 'vertex_label)).label
  }}

  /**
   * private functions: (monomorphized acces + utils)
   * TODO: do not export in the interface (OPA missing feature).
   * Please do not use in your code.
   */

  @private get_src(e) = (e : FGraph.edge).src

  @private get_dst(e) = (e : FGraph.edge).dst
  @private get_vertices(t) = (t : FGraph.t).vertices
  @private get_size(t) = (t : FGraph.t).size
  @private get_node(t, vertex) =
    vertices = get_vertices(t)
    VertexMap.get(vertex, vertices)

  @private cardinal_edges(inout) =
    fold(_, edges, acc) = List.length(edges) + acc
    VertexMap.fold(fold, inout, 0)

  @private cardinal_edges_label(inout, label) =
    fold(_, edges, acc) =
      fold(edge, acc) =
        if Order.equals((edge : FGraph.edge).label,label,  X.order_edge_label) then acc + 1
        else acc
      List.fold(fold, edges, acc)
    VertexMap.fold(fold, inout, 0)


  /**
   * {1 Size functions}
   */

  is_empty(t) = get_size(t) == 0
  nb_vertices = get_size

  nb_edges(t) =
    vertices = get_vertices(t)
    fold(_, node, acc) = cardinal_edges(node.out) + acc
    VertexMap.fold(fold, vertices, 0)

  /**
   * Compute the out (resp. in) degree of a vertex [vertex] ignoring labels.
   * This is the number of vertices pointed by (resp which points to) [vertex] with at least 1 edge.
   * If the vertex is not in the graph, the function returns [0].
   */

  out_neighbors(t, vertex) =
    match get_node(t, vertex) with
    | { none } -> 0
    | { some = node } -> VertexMap.size(node.out)

  in_neighbors(t, vertex) =
    match get_node(t, vertex) with
    | { none } -> 0
    | { some = node } -> VertexMap.size(node.in)

  /**
   * Computing the out (resp. in) degree of a vertex [vertex] distinguing edges by their labels.
   * If there are several edges between 2 vertices, they are counted several times.
   * If the vertex is not in the graph, the function returns [0].
   */

  out_degree(t, vertex) =
    match get_node(t, vertex) with
    | { none } -> 0
    | { some = node } -> cardinal_edges(node.out)

  in_degree(t, vertex) =
    match get_node(t, vertex) with
    | { none } -> 0
    | { some = node } -> cardinal_edges(node.in)

  /**
   * Computing the out (resp. in) degree of a vertex [vertex] filtered by a specific given label.
   * If the vertex is not in the graph, the function returns [0].
   */

  out_degree_label(t, vertex, label) =
    match get_node(t, vertex) with
    | { none } -> 0
    | { some = node } -> cardinal_edges_label(node.out, label)

  in_degree_label(t, vertex, label) =
    match get_node(t, vertex) with
    | { none } -> 0
    | { some = node } -> cardinal_edges_label(node.in, label)

  /**
   * {1 Membership functions}
   */

  /**
   * Test the occurence of a vertex in a graph.
   * Remember that vertex_label are ignored by the comparaison function.
   */
  mem_vertex(t, vertex) =
    vertices = get_vertices(t)
    VertexMap.mem(vertex, vertices)

  /**
   * Test the oriented connection between 2 vertices in a graph.
   * If one of the vertices in not in the graph, the function returns [false]
   */
  mem_neighbor(t, src, dst) =
    vertices = get_vertices(t)
    match VertexMap.get(src, vertices) with
    | { some = node } -> VertexMap.mem(dst, node.out)
    | { none } -> false

  /**
   * Test the occurence of a labeled edge between 2 vertices in a graph with a specific label
   * If one of the vertices in not in the graph, the function returns [false]
   */
  mem_edge(t, src, label, dst) =
    vertices = get_vertices(t)
    match VertexMap.get(src, vertices) with
    | { some = node } ->
      match VertexMap.get(dst, node.out) with
      | { none } -> false
      | { some = edges } ->
        // TODO: the list is ordered wrt the X.compare_edge_label function, this can be optimized
        // This has probably no impact for small list of edges
        exists(edge) = Order.equals(edge.label, label, X.order_edge_label)
        List.exists(exists, edges)
      end
    | { none } -> false

  /**
   * Sugar of interface for [mem_edge_label], working on an edge.
   */
  mem_edge_e(t, (edge : FGraph.edge)) = mem_edge(t, edge.src, edge.label, edge.dst)

  /**
   * Find the list of all edges between 2 vertices. If one a the vertices is not in the graph, returns [List.empty]
   */
  find_edge(t, src, dst) =
    vertices = get_vertices(t)
    match VertexMap.get(src, vertices) with
    | { some = node } -> VertexMap.get(dst, node.out) ? List.empty
    | { none } -> List.empty

  /**
   * {1 Successors and predecessors}
   */

  /**
   * Do not construct these lists for iteration, but rather use directly FGraph iterators.
   */

  /**
   * Returns the successors (resp. predecessors) of a [vertex].
   * The returned list does not contains doublons, although they could be several
   * edges between the [vertices]. The returned list is empty if the vertex is not in the graph.
   */

  succ(t, src) =
    match get_node(t, src) with
    | { none } -> List.empty
    | { some = node } -> VertexMap.To.key_list(node.out)

  pred(t, dst) =
    match get_node(t, dst) with
    | { none } -> List.empty
    | { some = node } -> VertexMap.To.key_list(node.in)

  /**
   * Same, successors / predecessors but restricting the label of the edges
   */

  succ_label(t, src, label) =
    match get_node(t, src) with
    | { none } -> List.empty
    | { some = node } ->
      fold(_, edges, acc) =
        fold(edge, acc) =
          if Order.equals((edge : FGraph.edge).label, label,  X.order_edge_label) then List.cons(edge.dst, acc)
          else acc
        List.fold(fold, edges, acc)
      VertexMap.fold(fold, node.out, List.empty)

  pred_label(t, dst, label) =
    match get_node(t, dst) with
    | { none } -> List.empty
    | { some = node } ->
      fold(_, edges, acc) =
        fold(edge, acc) =
          if Order.equals((edge : FGraph.edge).label, label, X.order_edge_label) then List.cons(edge.src, acc)
          else acc
        List.fold(fold, edges, acc)
      VertexMap.fold(fold, node.in, List.empty)

  succ_e(t, src) =
    match get_node(t, src) with
    | { none } -> List.empty
    | { some = node } -> VertexMap.To.val_list(node.out)

  pred_e(t, dst) =
    match get_node(t, dst) with
    | { none } -> List.empty
    | { some = node } -> VertexMap.To.val_list(node.in)

  /**
   * {1 Graph iterators}
   */

  /**
   * Iter on all vertices of a graph.
   */
  iter_vertex(iter, t) =
    vertices = get_vertices(t)
    VertexMap.iter(vertex, _ -> iter(vertex), vertices)

  /**
   * Fold on all vertices of a graph.
   */
  fold_vertex(fold, t, acc) =
    vertices = get_vertices(t)
    VertexMap.fold(vertex, _, acc -> fold(vertex, acc), vertices, acc)

  /**
   * Iter on all edges.
   */
  iter_edge(iter, t) =
    iter(node) =
      out = (node : FGraph.node).out
      VertexMap.iter(_, edges -> List.iter(edge -> iter(edge.src, edge.label, edge.dst), edges), out)
    vertices = get_vertices(t)
    VertexMap.iter(_, node -> iter(node), vertices)

  /**
   * Fold on all edges.
   */
  fold_edge(fold, t, acc) =
    fold(node, acc) =
      out = (node : FGraph.node).out
      VertexMap.fold(_, edges, acc -> List.fold(edge, acc -> fold(edge.src, edge.label, edge.dst, acc), edges, acc), out, acc)
    vertices = get_vertices(t)
    VertexMap.fold(_, node, acc -> fold(node, acc), vertices, acc)

  /**
   * Iter on all neighbors. There is only one iteration, even if there are several edges between 2 vertices
   */
  iter_neighbor(iter, t) =
    iter(src, node) =
      out = (node : FGraph.node).out
      VertexMap.iter(dst, _ -> iter(src, dst), out)
    vertices = get_vertices(t)
    VertexMap.iter(iter, vertices)

  /**
   * Fold on all neighbors.
   * There is only one iteration for an oriented connection,
   * even if there are several edges between 2 vertices
   */
  fold_neighbor(fold, t, acc) =
    fold(src, node, acc) =
      out = (node : FGraph.node).out
      VertexMap.fold(dst, _, acc -> fold(src, dst, acc), out, acc)
    vertices = get_vertices(t)
    VertexMap.fold(fold, vertices, acc)

  /**
   * Sugar of interface for [iter_edge], working on [FGraph.edge]
   */
  iter_edge_e(iter, t) =
    iter(node) =
      out = (node : FGraph.node).out
      VertexMap.iter(_, edges -> List.iter(iter, edges), out)
    vertices = get_vertices(t)
    VertexMap.iter(_, node -> iter(node), vertices)

  /**
   * Sugar of interface for [fold_edge], working on [FGraph.edge]
   */
  fold_edge_e(fold, t, acc) =
    fold(node, acc) =
      out = (node : FGraph.node).out
      VertexMap.fold(_, edges, acc -> List.fold(fold, edges, acc), out, acc)
    vertices = get_vertices(t)
    VertexMap.fold(_, node, acc -> fold(node, acc), vertices, acc)

  /**
   * {1 Vertex iterators}
   */

  /**
   * Iter/fold on all successors/predecessors of a vertex. There is no doublon in iteration, even if there are several
   * edges between 2 vertices.
   */

  iter_succ(iter, t, src) =
    match get_node(t, src) with
    | { none } -> void
    | { some = node } ->
      iter(dst, _) = iter(dst)
      VertexMap.iter(iter, node.out)

  iter_pred(iter, t, dst) =
    match get_node(t, dst) with
    | { none } -> void
    | { some = node } ->
      iter(src, _) = iter(src)
      VertexMap.iter(iter, node.in)

  fold_succ(fold, t, src, acc) =
    match get_node(t, src) with
    | { none } -> void
    | { some = node } ->
      fold(dst, _, acc) = fold(dst, acc)
      VertexMap.fold(fold, node.out, acc)

  fold_pred(fold, t, dst, acc) =
    match get_node(t, dst) with
    | { none } -> void
    | { some = node } ->
      fold(src, _, acc) = fold(src, acc)
      VertexMap.fold(fold, node.in, acc)

  iter_succ_e(iter, t, src) =
    match get_node(t, src) with
    | { none } -> void
    | { some = node } ->
      iter(_, edges) = List.iter(iter, edges)
      VertexMap.iter(iter, node.out)

  iter_pred_e(iter, t, dst) =
    match get_node(t, dst) with
    | { none } -> void
    | { some = node } ->
      iter(_, edges) = List.iter(iter, edges)
      VertexMap.iter(iter, node.in)

  fold_succ_e(fold, t, src, acc) =
    match get_node(t, src) with
    | { none } -> void
    | { some = node } ->
      fold(_, edges, acc) = List.fold(fold, edges, acc)
      VertexMap.fold(fold, node.out, acc)

  fold_pred_e(fold, t, dst, acc) =
    match get_node(t, dst) with
    | { none } -> void
    | { some = node } ->
      fold(_, edges, acc) = List.fold(fold, edges, acc)
      VertexMap.fold(fold, node.in, acc)

  /**
   * If needed, we can add iter_succ_label, fold_succ_label
   */

  /**
   * {1 Add / Remove operations}
   */

  /**
   * The empty persistent graph
   */
  empty = { vertices = VertexMap.empty ; size = 0 } : FGraph.t

  /**
   * Add a vertex to a graph. Initially, this vertex has no neighbor.
   */
  add_vertex(t, vertex) =
    vertices = get_vertices(t)
    if VertexMap.mem(vertex, vertices) then t
    else
      size = t.size + 1
      in = VertexMap.empty
      out = VertexMap.empty
      node = ~{ in out }
      vertices = VertexMap.add(vertex, node, vertices)
      ~{ vertices size }

  /**
   * Remove a vertex from a graph.
   * Remove also all the edges related to this vertex.
   */
  remove_vertex(t, vertex) =
    vertices = get_vertices(t)
    if VertexMap.mem(vertex, vertices)
    then
      remove_edge(edges) =
        fold(v2, edges, acc) =
          filter(edge : FGraph.edge) =
            not(Vertex.equal(vertex, edge.src) || Vertex.equal(vertex, edge.dst))
          edges = List.filter(filter, edges)
          VertexMap.add(v2, edges, acc)
        VertexMap.fold(fold, edges, VertexMap.empty)
      remove_node(node : FGraph.node) =
        in = remove_edge(node.in)
        out = remove_edge(node.out)
        ~{ node with in out }
      vertices =
        fold(v2, node, acc) =
          if Vertex.equal(vertex, v2) then acc
          else
            node = remove_node(node)
            VertexMap.add(v2, node, acc)
        VertexMap.fold(fold, vertices, VertexMap.empty)
      size = t.size - 1
      ~{ vertices size }

    else t

  /**
   * Add a edge to a graph. If the vertices are not yet in the graph, add the vertices too.
   */
  add_edge_e(t, edge) =
    vertices = get_vertices(t)
    src = (edge : FGraph.edge).src
    dst = edge.dst
    size = t.size
    do jlog("add_edge:HOOK-01")

    (size, src_node) =
      match VertexMap.get(src, vertices) with
      | { none } ->
        in = VertexMap.empty
        out = VertexMap.singleton(dst, [edge])
        size = size + 1
        node = ~{ in out }
        (size, node)

      | { some = node } ->
        do jlog("add_edge:HOOK-02")
        edges = VertexMap.get(dst, node.out) ? List.empty
        do jlog("Size before : {List.length(edges)}")
        edges = List.add_uniq(Edge.ordering_label, edge, edges)
        do jlog("Size after : {List.length(edges)}")
        out = VertexMap.add(dst, edges, node.out)
        node = ~{ node with out }
        (size, node)

    (size, dst_node) =
      match VertexMap.get(dst, vertices) with
      | { none } ->
        in = VertexMap.singleton(src, [edge])
        out = VertexMap.empty
        size = size + 1
        node = ~{ in out }
        (size, node)

      | { some = node } ->
        do jlog("add_edge:HOOK-03")
        edges = VertexMap.get(src, node.in) ? List.empty
        do jlog("Size before : {List.length(edges)}")
        edges = List.add_uniq(Edge.ordering_label, edge, edges)
        do jlog("Size after : {List.length(edges)}")
        in = VertexMap.add(src, edges, node.in)
        node = ~{ node with in }
        (size, node)

    vertices = VertexMap.add(src, src_node, vertices)
    vertices = VertexMap.add(dst, dst_node, vertices)
    ~{ vertices size }

  /**
   * A Sugar of interface. See [add_edge_e]
   */
  add_edge(t, src, label, dst) =
    edge = ~{ src label dst }
    add_edge_e(t, edge)

  /**
   * Remove an edge of the graph. If the edge is not in the graph, returns the same graph.
   * FIXME: we can return physically the same graph, if we have the fonction List.filter_stable.
   * If one of the vertices is not in the graph, the function considere just
   * that the edge is not in the graph (no error).
   */
  remove_edge(t, src, label, dst) =
    vertices = get_vertices(t)
    initial_vertices = vertices

    exists(edge) = Order.equals((edge : FGraph.edge).label, label, X.order_edge_label)

    vertices =
      match VertexMap.get(src, vertices) with
      | { none } -> vertices
      | { some = node } ->
        match VertexMap.get(dst, node.out) with
        | { none } -> vertices
        | { some = edges } ->
          // replace by filter stable
          if List.exists(exists, edges)
          then
            edges = List.remove_p(exists, edges)
            out = VertexMap.add(dst, edges, node.out)
            node = ~{ node with out }
            VertexMap.add(src, node, vertices)
          else vertices

    vertices =
      match VertexMap.get(dst, vertices) with
      | { none } -> vertices
      | { some = node } ->
        match VertexMap.get(src, node.in) with
        | { none } -> vertices
        | { some = edges } ->
          // replace by filter stable
          if List.exists(exists, edges)
          then
            edges = List.remove_p(exists, edges)
            in = VertexMap.add(src, edges, node.in)
            node = ~{ node with in }
            VertexMap.add(dst, node, vertices)
          else vertices

    if vertices === initial_vertices then t
    else
      ~{ t with vertices }

  /**
   * A Sugar of interface. See [remove_edge]
   */
  remove_edge_e(t, edge : FGraph.edge) =
    remove_edge(t, edge.src, edge.label, edge.dst)

  /**
   * Remove all oriented edges between 2 vertices from [src] to [dst].
   * Returns the same graph if not any edges exists between the 2 vertices.
   */
  remove_all_edge(t, src, dst) =
    vertices = get_vertices(t)
    initial_vertices = vertices

    vertices =
      match VertexMap.get(src, vertices) with
      | { none } -> vertices
      | { some = node } ->
        out = VertexMap.add(dst, List.empty, node.out)
        node = ~{ node with out }
        VertexMap.add(src, node, vertices)

    vertices =
      match VertexMap.get(dst, vertices) with
      | { none } -> vertices
      | { some = node } ->
        in = VertexMap.add(src, List.empty, node.in)
        node = ~{ node with in }
        VertexMap.add(dst, node, vertices)

    if vertices === initial_vertices then t
    else
      ~{ t with vertices }


  /**
   *
   */
  Export = {{
    to_dot(t) =
      text = Text.cons("# OPA graph : FGraph.Export.to_dot \n")
      text = Text.insert_right(text, "digraph G \{\n")
      fold(vertex : FGraph.vertex, text) =
        label = X.string_of_vertex_label(vertex.label)
        i = vertex.index
        Text.insert_right(text, "{i} [label=\"{label}\"]\n")
      text = fold_vertex(fold, t, text)
      fold(edge : FGraph.edge, text) =
        src = edge.src.index
        dst = edge.dst.index
        label = X.string_of_edge_label(edge.label)
        Text.insert_right(text, "{src} -> {dst} [label=\"{label}\"]\n")
      text = fold_edge_e(fold, t, text)
      text = Text.insert_right(text, "}\n")
      Text.to_string(text)

    to_xhtml(t) =
      dot = to_dot(t)
      Dot.to_xhtml(dot)
  }}

}}

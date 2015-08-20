

/**
 * @author MBLACK88
 */
object dijkstra {
  val DEBUG : Boolean = false  
  val INF = Int.MaxValue
  
  //***********************************************************************
  // Generic Print List Function to just "Dump" a List[Map[] to the console
  //***********************************************************************
  def printlist (network : List[Map[String,Any]]) = {
    network.foreach (n => println(n))
  } // END: printlist
  
  //***********************************************************************
  //  Given an network map as input, generate a unique list of Node names and 
  //  corresponding ID to them.
  //  This will be used to map node names to array positions during the calculations
  //***********************************************************************
  
  def buildNodeList(network : List[Map[String,Any]]) : List[Map[String,Any]]= {
    var nodelist : List[String] = Nil
    var mappedlist : List[Map[String,Any]] = Nil
    
    // Get all the node names from the network map
    // This will include duplicates
    network.foreach { n =>
      nodelist ::= n("startLocation").asInstanceOf[String]
      nodelist ::= n("endLocation").asInstanceOf[String]
    }
    
    // Remove the duplicate names fromt he nodelist and sort them.
    nodelist = nodelist.distinct.sorted
    
    // Initialize our node id number
    var i=0
    // Loop over all the items in the node list and build the return List[Map[]]
    nodelist.foreach {n => 
      mappedlist ::= Map("node"->n,"nodepos"->i)
      i+=1
    }
    // Send back the sorted mapped list to the caller.
    return mappedlist
    
  } // END: buildNodeList

  //***********************************************************************
  // Utility function to validate input map for valid keys 
  //***********************************************************************
  def ValidateKeys(network : List[Map[String,Any]]) : Boolean = {
    var valid = true
    network.foreach { node =>
      if (valid == true) {
        val nodekeys = node.keys
        nodekeys.foreach{ key =>
          if (valid == true) {
            valid = key match {
              case "startLocation" => true
              case "endLocation" => true
              case "distance" => true
              case _ => false
            }
          }
        }
      }
    }
    valid
  }  // END: ValidateKeys
  //***********************************************************************
  // Utility function to validate input map for valid distances
  //***********************************************************************
  def ValidateDistance(network : List[Map[String,Any]]) : Boolean = {
    var valid = true
    
    if ( network.filter(_("distance").asInstanceOf[Integer] <= 0).length > 0){
      valid = false
    }
    return valid
  }
  //***********************************************************************
  // Utility function to validate input map for consistancy
  // There should be no more than 1 path between a start/end combination
  //***********************************************************************
  def ValidateMapConsistency(network : List[Map[String,Any]]) : Boolean = {
    var valid = true
    
    network.foreach { n => 
      if (network.filter(_("startLocation") == n("startLocation")).filter(_("endLocation") == n("endLocation")).length > 1) valid = false
      
    }
    return valid
  }
  //***********************************************************************
  //
  // Function: getMinDistIdx
  // Return the node index of the node with the lowset distance that
  //   hasn't been visited
  //
  //***********************************************************************
  def getMinDistIdx(distance : Array[Integer],visited : Array[Boolean]) : Integer = {
    var min_dist = INF
    var min_idx = 0
    
    for(i <- 0 to (distance.size - 1)) {
      if (visited(i) == false && distance(i) <= min_dist) {
        min_dist = distance(i)
        min_idx = i
      }
    }
    return min_idx
  }
  //***********************************************************************
  //
  // Function: getEdgeValue
  // Return the node index of the node with the lowset distance that
  //   hasn't been visited
  //
  //***********************************************************************
  def getEdgeValue(networkmap : List[Map[String,Any]],start : String, end : String) : Integer = {

  var retVal = -1 // Since edge weights are positive a Negative number signals an error

  val nodes = networkmap.filter( _("startLocation") == start ).filter(_("endLocation") == end)

  if (nodes.length == 1) {
    retVal = nodes(0)("distance").asInstanceOf[Integer]
  }
  else if (nodes.length == 0){
    retVal = 0
  }
  else {
    retVal = -1
  }
  return retVal
} // End getEdgeValue
  //***********************************************************************
  //
  // Function: dijkstra
  //
  //***********************************************************************
  def dijkstra (network : List[Map[String,Any]],startnode : String, endnode : String) : Integer = {
    
    
    if (ValidateKeys(network) == false) {
      // If we don't a good network map then just bail out.
      println("Network Map contains bad key(s)")
      return -1
    }
    
    if (ValidateDistance(network) == false) {
      // If there are distances that are not valid then bail out
      println("Network Map contains distances that are either negative or zero")
      return -3
    }
    
    if (ValidateMapConsistency(network) == false) {
      // If map isn't consistant then bail out.
      println("Network not consistant multiple paths between some nodes")
      return -4
    }
    
    var nodemap : List[Map[String,Any]] = Nil
    
    // Initialize starting Data Structures
    nodemap = buildNodeList(network)
    
    // If the start or end node not in the network map then bail out.
    if ( (nodemap.filter(_("node")==startnode).length < 1) ||
         (nodemap.filter(_("node")==endnode).length < 1))  {
      println(s" ${startnode} or ${endnode} not defined in the network map")
      return -2
    }
    
    // For readability save the number of vertex (Nodes) we have in the network
    val NUM_VERTEX = nodemap.size
    var distance : Array[Integer]     = new Array[Integer](NUM_VERTEX)      // Min Dist to a node from "Start"
    var previous : Array[Integer]     = new Array[Integer](NUM_VERTEX)      // Previous Node getting to the "Index" Node
    var visited  : Array[Boolean]     = new Array[Boolean](NUM_VERTEX)  // Have we been to this node before 
    
    // Initialize the visitied array to "False"
    // Intialize the distance array to Infinity
    for (i <- 0 to NUM_VERTEX-1) {
      distance(i) = INF   
      visited(i) = false
    }
    // Setup the min distance to our start node
    // We Control nodemap and we know that it is a unique list so (0) is 
    // is safe to use for the result of "Filter" 
    distance(nodemap.filter(_("node") == startnode)(0)("nodepos").asInstanceOf[Integer]) = 0
    
    for ( node <- 0 to NUM_VERTEX - 1){
      // Set the prev_vertex to the starting node
      // and set as already visited.
      // By default the distance from the start to the start is 0
      var prev_vertex = getMinDistIdx(distance,visited)
      visited(prev_vertex) = true
      
      //Everything is now setup and we can start looking for the shortest distance
      //from the startnoe to the endnode
      
      for(curr_vertex <- 0 to NUM_VERTEX -1) {
        val start_node = nodemap.filter(_("nodepos") == prev_vertex)(0)("node").asInstanceOf[String]
        val end_node   = nodemap.filter(_("nodepos") == curr_vertex)(0)("node").asInstanceOf[String]
        
        // This is the bulk of the Logic
        // If we haven't visited this node already
        // and there is a path from the node to node
        // and our last node's distance isn't infinite
        // and the distance of the last node + the distance from node to node is "Less" than the current nodes distance
        // Then set the current vertex's distance to the last nodes distance plus the distance of the node to node edge
        //    and set the current nodes previous node to the previous node
        if ( ! visited(curr_vertex) && 
             (getEdgeValue(network,start_node,end_node) > 0 ) &&
             distance(prev_vertex) != INF && 
             distance(prev_vertex)+getEdgeValue(network,start_node,end_node) < distance(curr_vertex)){
             distance(curr_vertex) = distance(prev_vertex) + getEdgeValue(network,start_node,end_node)
             previous(curr_vertex) = prev_vertex
        }
      }
    }
    
    //
    // Ok Time To See if we can make it to the endnode
    // Bail out of here
    //
    if ( distance(nodemap.filter(_("node") == endnode)(0)("nodepos").asInstanceOf[Integer]) == INF) {
        println(s"There is no path from ${startnode} to ${endnode}")
        return -3
    }
    // 
    // Just a little debug I didn't want to remove just yet.
    //
    if (DEBUG) {
      println("Distance Array")
      for (i <- 0 to (distance.size - 1)) {println(distance(i) + " " + nodemap.filter(_("nodepos") == i)(0)("node").asInstanceOf[String])}
      println("Visited Array")
      for (i <- 0 to (visited.size - 1)) {println(i + " " + visited(i))}
      println("Previous Array")
      for (i <- 0 to (previous.size - 1)) {println(i + " " + nodemap.filter(_("nodepos") == i)(0)("node").asInstanceOf[String] + " " + previous(i))}
    }
    // 
    // Print Out The Results
    //
    var current_node  : Integer = nodemap.filter(_("node")==endnode)(0)("nodepos").asInstanceOf[Integer] 
    val end_node      : Integer =  nodemap.filter(_("node")==startnode)(0)("nodepos").asInstanceOf[Integer]  
    
    var path : String = nodemap.filter(_("nodepos") == current_node)(0)("node").asInstanceOf[String]
    current_node = previous(current_node)
    while (current_node != end_node){
      path = nodemap.filter(_("nodepos") == current_node)(0)("node").asInstanceOf[String] + " => " + path
      current_node = previous(current_node)
    }
    path = nodemap.filter(_("nodepos") == current_node)(0)("node").asInstanceOf[String] + " => " + path

  val shortest_dist = distance(nodemap.filter(_("node") == endnode)(0)("nodepos").asInstanceOf[Integer])
  val outstring : String = "Map(\"distance\" -> " + shortest_dist + ",  \"path\" -> \"" + path + "\")"
  println(outstring)
  
  return 0
  }  // End dijkstra 

}  //END dijkstra Object
##############################################################################
# Libraries
##############################################################################
library(networkD3)
library(htmlwidgets)

##############################################################################
# Nodes
##############################################################################
nodes <- data.frame(
  name = c(
    "Municipal Solid Waste|(100%)",
    "Biodegradable Organic Fraction|(45.3%)",
    "Dry Recyclables|(33.6%)",
    "Residual Waste and Others|(21.1%)",
    "Anaerobic Digestion",
    "Composting",
    "Sorting and Recycling",
    "Combustion",
    "Disposal",
    "Biogas",
    "Compost",
    "Recovered Materials",
    "Electricity/Heat",
    "Ash and Residual Waste"
  ),
  group = c(
    "source",
    "organic",
    "recyclable",
    "residual",
    "digestion",
    "composting",
    "recycling",
    "combustion",
    "disposal",
    "biogas",
    "compost",
    "recovered",
    "energy",
    "ash"
  )
)

##############################################################################
# Flow
##############################################################################
links <- data.frame(
  source = c(
    0, 0, 0,
    1, 1,
    2,
    3, 3,
    4, 5, 6,
    7, 7
  ),
  
  target = c(
    1, 2, 3,
    4, 5,
    6,
    7, 8,
    9, 10, 11,
    12, 13
  ),
  
  value = c(
    45.3, 33.6, 21.1,
    10.0, 35.3,
    33.6,
    10.0, 11.1,
    10.0, 35.3, 33.6,
    7.5, 2.5
  ),
  
  group = c(
    "organic",
    "recyclable",
    "residual",
    "digestion",
    "composting",
    "recycling",
    "combustion",
    "disposal",
    "biogas",
    "compost",
    "recovered",
    "energy",
    "ash"
  )
)

##############################################################################
# Colors
##############################################################################
colourScale <- '
d3.scaleOrdinal()
.domain([
  "source", "organic", "recyclable", "residual",
  "digestion", "composting", "recycling",
  "combustion", "disposal", "biogas",
  "compost", "recovered", "energy", "ash"
])
.range([
  "#0066AD",
  "#168548",
  "#159B91",
  "#E47A60",
  "#0BAA46",
  "#69A70B",
  "#13A8E8",
  "#E17B00",
  "#737B83",
  "#0BAA46",
  "#69A70B",
  "#13A8E8",
  "#F59B0B",
  "#B18A6A"
])
'

##############################################################################
# Label Formatting
##############################################################################
sankey <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = "group",
  LinkGroup = "group",
  colourScale = colourScale,
  fontSize = 16,
  fontFamily = "Arial",
  nodeWidth = 20,
  nodePadding = 25,
  sinksRight = TRUE,
  iterations = 128,
  width = 1200,
  height = 800,
  margin = list(
    top = 20,
    right = 180,
    bottom = 20,
    left = 20
  ),
  units = "%"
)

sankey <- onRender(
  sankey,
  
  '
  function(el, x) {

    d3.select(el)
      .selectAll(".node text")
      .each(function() {

        var element = d3.select(this);
        var label = element.text();
        var parts = label.split("|");

        if (parts.length > 1) {
          var xPos = element.attr("x");
          element.text(null);
          element.append("tspan")
            .attr("x", xPos)
            .attr("dy", "0em")
            .text(parts[0]);
          element.append("tspan")
            .attr("x", xPos)
            .attr("dy", "1.3em")
            .style("font-size", "16px")
            .style("font-weight", "normal")
            .text(parts[1]);
        }
      });
  }
  '
)

##############################################################################
# Plot
##############################################################################
sankey


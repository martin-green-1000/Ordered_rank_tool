# Ordered_rank_tool
# WORKING
docker build -t order_rank_tool . ### BUILDS IMAGE
docker run -d --rm -p 5648:5648 order_rank_tool ## RUNS IMAGE IN CONTAINER


### second version with ties

docker build -t order_rank_tool_ties . ### BUILDS IMAGE
docker run -d --rm -p 5648:5648 order_rank_tool_ties ## RUNS IMAGE IN CONTAINER

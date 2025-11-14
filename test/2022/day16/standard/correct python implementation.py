#!/usr/bin/env python
import re
from itertools import product

input_file = "day16 (data).csv"

lines_infos = [re.findall(r"[A-Z][A-Z]|\d+", line) for line in open(input_file).read().splitlines()]

graph = {x[0]: set(x[2:]) for x in lines_infos}
flow = {x[0]: int(x[1]) for x in lines_infos if int(x[1]) != 0}
encoded_valves = {x: i for i, x in enumerate(flow)}
shortest_path = {x: {y: 1 if y in graph[x] else float("+inf") for y in graph} for x in graph}
for mid, src, dst in product(graph, graph, graph):
    shortest_path[src][dst] = min(shortest_path[src][dst], shortest_path[src][mid] + shortest_path[mid][dst])

def dp(current, remaining_time, visited, path, current_flow, outcomes):
    visited_tuple = tuple([1 if x > 0 else 0 for x in visited])
    if visited_tuple not in outcomes or current_flow > outcomes[visited_tuple][0]:
        outcomes[visited_tuple] = (current_flow, path.copy())
    for dest, index in encoded_valves.items():
        time_after_dest = remaining_time - shortest_path[current][dest] - 1
        if visited[index] > 0 or time_after_dest <= 0:
            continue
        new_visited = visited.copy()
        new_visited[index] = len(path) + 1  # Record the order of visit
        new_path = path.copy()
        new_path.append(dest)  # Add the destination to the path
        dp(dest, time_after_dest, new_visited, new_path, current_flow + time_after_dest * flow[dest], outcomes)
    return outcomes

initial_visited = [0] * len(encoded_valves)
initial_path = []
maxFlowedForVisitedPart1 = dp("AA", 30, initial_visited, initial_path, 0, {})

# Manually find the maximum value for maxFlowedProblemPart1 and its corresponding path
maxFlowedProblemPart1 = float("-inf")
best_path1 = []
for value, path in maxFlowedForVisitedPart1.values():
    if value > maxFlowedProblemPart1:
        maxFlowedProblemPart1 = value
        best_path1 = path

print("Max flow for maxFlowedProblemPart1:", maxFlowedProblemPart1)
print("Best path:", best_path1)

maxFlowedForVisitedPart2 = dp("AA", 26, initial_visited, initial_path, 0, {})

# Manually find the maximum value for the second print statement and its corresponding paths
maxFlowedProblemPart2 = float("-inf")
best_paths = []
for k1, (v1, path1) in maxFlowedForVisitedPart2.items():
    for k2, (v2, path2) in maxFlowedForVisitedPart2.items():
        if not any(a & b for a, b in zip(k1, k2)):
            current_sum = v1 + v2
            if current_sum > maxFlowedProblemPart2:
                maxFlowedProblemPart2 = current_sum
                best_paths = [path1, path2]

print("Max flow for maxFlowedProblemPart2:", maxFlowedProblemPart2)
print("Best paths:", best_paths)
import time

type Node = str
type Graph = dict[Node, list[Node]]


def parse_input(input: str) -> Graph:
    return {
        source: targets.split()
        for line in input.strip().split("\n")
        for source, targets in [line.split(": ")]
    }


def count_paths(start: Node, graph: Graph, target: Node = "out") -> int:
    memo = {}

    def dfs(node: Node) -> int:
        if node == target:
            return 1

        if node in memo:
            return memo[node]

        memo[node] = sum(dfs(neighbor) for neighbor in graph.get(node, []))

        return memo[node]

    return dfs(start)


def count_paths_visiting(
    start: Node,
    graph: Graph,
    req: set[Node],
    target: Node = "out",
) -> int:
    memo = {}
    req_frz = frozenset(req)

    def dfs(node: Node, req_visited: frozenset[Node]) -> int:
        state = (node, req_visited)
        if state in memo:
            return memo[state]

        if node == target:
            result = 1 if req_visited == req_frz else 0
            memo[state] = result
            return result

        new_visited = req_visited | ({node} if node in req else set())

        memo[state] = sum(
            dfs(neighbor, new_visited) for neighbor in graph.get(node, [])
        )
        return memo[state]

    return dfs(start, frozenset())


if __name__ == "__main__":
    t0 = time.perf_counter()
    with open("input") as f:
        input = f.read()
    graph = parse_input(input)
    t1 = time.perf_counter()
    print(f"parse: {(t1 - t0) * 1000:.2f}ms")

    ans_p1 = count_paths("you", graph)
    t2 = time.perf_counter()
    print(f"p1: {(t2 - t1) * 1000:.2f}ms ({ans_p1=})")  # 0.05ms | 431

    ans_p2 = count_paths_visiting("svr", graph, req={"dac", "fft"})
    t3 = time.perf_counter()
    print(f"p2: {(t3 - t2) * 1000:.2f}ms ({ans_p2=})")  # 0.99ms | 358458157650450

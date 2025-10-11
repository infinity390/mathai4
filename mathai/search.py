from mathai import *
import copy
from concurrent.futures import ThreadPoolExecutor, TimeoutError

# Original expression
original_eq = simplify(parse("((P|~Q)&(P&~Q|P&R)&(~P&~R|~Q))<->P&~Q"))

# Simplification functions
lst = [logic1, logic3, logic2]

MAX_SMALL = 2
smallest_four = []

# Stack element: (current_expr, path_list)
stack = [(copy.deepcopy(original_eq), [copy.deepcopy(original_eq)])]

# Keep track of visited expressions to prevent cycles
visited = set()

# Boolean constants for immediate termination
TRUE_EXPR = tree_form("s_true")
FALSE_EXPR = tree_form("s_false")

found_boolean = False
boolean_path = None
boolean_expr = None

# Thread pool executor
executor = ThreadPoolExecutor(max_workers=3)

while stack and not found_boolean:
    current_eq, path = stack.pop()
    expr_str = str(current_eq)

    if expr_str in visited:
        continue
    visited.add(expr_str)

    # Thinking message
    printeq(current_eq)

    # Immediate termination for boolean constants
    if current_eq == TRUE_EXPR or current_eq == FALSE_EXPR:
        found_boolean = True
        boolean_path = path
        boolean_expr = current_eq
        break

    # Insert into smallest_four if qualifies
    inserted = False
    for j in range(len(smallest_four)):
        if len(expr_str) < len(str(smallest_four[j][0])):
            smallest_four.insert(j, (copy.deepcopy(current_eq), copy.deepcopy(path)))
            inserted = True
            break
    if not inserted and len(smallest_four) < MAX_SMALL:
        smallest_four.append((copy.deepcopy(current_eq), copy.deepcopy(path)))
    if len(smallest_four) > MAX_SMALL:
        smallest_four = smallest_four[:MAX_SMALL]

    # First, try functions that reduce length
    reduced_any = False
    for fx in lst:
        print(f"[Thinking] Executing {fx.__name__} on current expression:")
        printeq(current_eq)
        future = executor.submit(fx, current_eq)
        try:
            new_expr = future.result(timeout=5)
            new_expr_str = str(new_expr)
            # Only accept if shorter or equal
            if len(new_expr_str) <= len(expr_str) and new_expr_str != expr_str:
                reduced_any = True
                stack.append((new_expr, path + [copy.deepcopy(new_expr)]))
        except TimeoutError:
            print(f"[Thinking] {fx.__name__} timed out, skipping.")
            continue

    # If no reducing function produced a shorter or equal expression, try a “growing” function
    if not reduced_any:
        for fx in lst:
            print(f"[Thinking] Trying growing {fx.__name__} on current expression:")
            printeq(current_eq)
            future = executor.submit(fx, current_eq)
            try:
                new_expr = future.result(timeout=5)
                new_expr_str = str(new_expr)
                if new_expr_str != expr_str:
                    stack.append((new_expr, path + [copy.deepcopy(new_expr)]))
                    break  # only take one growing function
            except TimeoutError:
                print(f"[Thinking] {fx.__name__} (growing) timed out, skipping.")
                continue

# Shutdown executor
executor.shutdown(wait=True)

# Display final results
if found_boolean:
    print("\nBoolean constant found! Full path to solution:\n")
    for step in boolean_path:
        printeq(step)
else:
    print("\nDFS completed. Two smallest expressions with steps:\n")
    for expr, path in smallest_four:
        print("Path to final expression:")
        for step in path:
            printeq(step)
        print("-"*50)

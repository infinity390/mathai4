from mathai import *
import copy
from concurrent.futures import ThreadPoolExecutor, TimeoutError

def dfs_simplify(equation, functions, true_expr, false_expr,
                 max_timeout=25, max_small=4,
                 base_timeout=1, time_per_char=0.1, timeout_increase=0.5):
    """
    Perform DFS simplification on a given equation using provided functions.

    Args:
        equation: The starting expression (TreeNode or parsed equation)
        functions: List of simplification functions
        true_expr: Expression representing True (immediate termination)
        false_expr: Expression representing False (immediate termination)
        max_timeout: Maximum timeout allowed for any function
        max_small: Number of smallest expressions to track
        base_timeout: Base timeout in seconds
        time_per_char: Additional timeout per character of expression
        timeout_increase: Factor to increase timeout for consecutive timeouts

    Returns:
        tuple(found_boolean, boolean_path, smallest_expressions)
    """
    original_eq = simplify(equation)
    smallest_four = []

    stack = [(copy.deepcopy(original_eq), [copy.deepcopy(original_eq)])]
    visited = set()

    found_boolean = False
    boolean_path = None
    boolean_expr = None

    executor = ThreadPoolExecutor(max_workers=3)
    consecutive_timeouts = 0

    while stack and not found_boolean:
        current_eq, path = stack.pop()
        expr_str = str(current_eq)

        if expr_str in visited:
            continue
        visited.add(expr_str)

        # Thinking message
        printeq(current_eq)

        # Immediate termination for provided boolean expressions
        if current_eq == true_expr or current_eq == false_expr:
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
        if not inserted and len(smallest_four) < max_small:
            smallest_four.append((copy.deepcopy(current_eq), copy.deepcopy(path)))
        if len(smallest_four) > max_small:
            smallest_four = smallest_four[:max_small]

        # Calculate adaptive timeout with cap
        timeout = (base_timeout + time_per_char * len(expr_str)) * (1 + timeout_increase * consecutive_timeouts)
        if timeout > max_timeout:
            timeout = max_timeout

        # Try functions that reduce length first
        reduced_any = False
        for fx in functions:
            print(f"[Thinking] Executing {fx.__name__} on current expression (timeout={timeout:.2f}s):")
            printeq(current_eq)
            future = executor.submit(fx, current_eq)
            try:
                new_expr = future.result(timeout=timeout)
                new_expr_str = str(new_expr)
                if len(new_expr_str) <= len(expr_str) and new_expr_str != expr_str:
                    reduced_any = True
                    stack.append((new_expr, path + [copy.deepcopy(new_expr)]))
                consecutive_timeouts = 0  # reset after success
            except TimeoutError:
                print(f"[Thinking] {fx.__name__} timed out, skipping.")
                consecutive_timeouts += 1
                continue

        # If no reducing function worked, try growing functions
        if not reduced_any:
            for fx in functions:
                print(f"[Thinking] Trying growing {fx.__name__} on current expression (timeout={timeout:.2f}s):")
                printeq(current_eq)
                future = executor.submit(fx, current_eq)
                try:
                    new_expr = future.result(timeout=timeout)
                    new_expr_str = str(new_expr)
                    if new_expr_str != expr_str:
                        stack.append((new_expr, path + [copy.deepcopy(new_expr)]))
                        consecutive_timeouts = 0
                        break  # only take one growing function
                except TimeoutError:
                    print(f"[Thinking] {fx.__name__} (growing) timed out, skipping.")
                    consecutive_timeouts += 1
                    continue

    executor.shutdown(wait=True)

    return found_boolean, boolean_path, smallest_four

"""End-to-end abort test against a live GraphTalker + AllegroGraph.

Connects to a real triple store, sends a real Claude query,
aborts it 3 times, then lets the 4th run to completion.

Usage:
    python tests/test_abort_e2e.py
"""

import sys
import threading
import time

sys.path.insert(0, ".")
from franz.graphtalker import EvalError, GraphTalkerClient, QueryAbortedError


def main():
    client = GraphTalkerClient(port=8080)

    # Connect to local AllegroGraph
    print("Connecting to AllegroGraph healthcare repository...")
    output = client.connect(
        "http", "localhost", 10035, "", "healthcare", "test", "xyzzy"
    )
    print(f"  {output}")

    assert client.health_check(), "Server not healthy"
    print("  Health check OK\n")

    question = "pls tell me what is in this repository"
    abort_delay = 5  # seconds before aborting

    # --- Abort 3 times ---
    for attempt in range(1, 4):
        print(f"=== Attempt {attempt}: query + abort after {abort_delay}s ===")
        client.clear_conversation()

        error_holder = {}
        result_holder = {}

        def run_query():
            try:
                result = client.claude_query(
                    question, continue_conversation=False, timeout=120
                )
                result_holder["answer"] = result.answer
            except QueryAbortedError as e:
                error_holder["aborted"] = str(e)
            except Exception as e:
                error_holder["error"] = f"{type(e).__name__}: {e}"

        thread = threading.Thread(target=run_query)
        start = time.time()
        thread.start()

        time.sleep(abort_delay)

        print(f"  Sending abort...")
        abort_result = client.abort_query()
        print(f"  abort_query() returned: {abort_result}")

        thread.join(timeout=30)
        elapsed = time.time() - start

        assert not thread.is_alive(), "Query thread still alive after join!"

        if "aborted" in error_holder:
            print(f"  Got QueryAbortedError (good): {error_holder['aborted']}")
        elif "error" in error_holder:
            print(f"  Got unexpected error: {error_holder['error']}")
        elif "answer" in result_holder:
            print(f"  Query completed before abort (was fast enough)")
        else:
            print(f"  No result and no error?!")

        print(f"  Elapsed: {elapsed:.1f}s")

        # Verify server still works
        check = client.eval("(+ 1 2)")
        assert check.parsed == 3, f"Server broken after abort! Got: {check.parsed}"
        print(f"  Server health after abort: OK\n")

    # --- Let it run to completion ---
    print("=== Attempt 4: letting query run to completion ===")
    client.clear_conversation()
    start = time.time()
    result = client.claude_query(question, continue_conversation=False, timeout=300)
    elapsed = time.time() - start

    print(f"  Completed in {elapsed:.1f}s")
    print(f"  Answer length: {len(result.answer)} chars")
    print(f"  Answer preview: {result.answer[:200]}...")
    print()

    # Final health check
    assert client.health_check(), "Server not healthy at end"
    check = client.eval("(+ 1 2)")
    assert check.parsed == 3
    print("=== ALL TESTS PASSED ===")

    client.close()


if __name__ == "__main__":
    main()

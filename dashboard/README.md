# Car Thing Dashboard

A funny event report for ClueCon Drives Car.

# Building

There's no automatic build system. If you need to host the dashboard elsewhere,
or have it served publicly, there are some steps you'll need to take.

1. Open `car-thing.html` for editing.
2. Find the line that says `let websocket_url = "ws://localhost:7000/dashboard";`.
3. Correct the url.
4. Find the span with id `phone_number`.
5. Correct the phone number within the span.
6. Save the file.

# Running

1. If you're using it, connect the FPV receiver described in [the root README](../README.md).
2. Host `car-thing.html`, or open it in a browser.

# SignalWire Relay protocol

Some notes taken about the Relay protocol early in this project's development.

<wss://relay.signalwire.com>

```json
/**
 * Base "params"
 */
{
  "agent": "<agent>", // process.env.SDK_PKG_AGENT
  "version": {
    "major": 3,
    "minor": 0,
    "revision": 0
  },
  "authentication": {
    /** SignalWire project id, e.g. `a10d8a9f-2166-4e82-56ff-118bc3a4840f` */
    "project": "<project>",
    /** SignalWire project token, e.g. `PT9e5660c101cd140a1c93a0197640a369cf5f16975a0079c9` */
    "token": "<token>" // or a JWT
  },
  "protocol": "<protocol>", // add from original RPC auth response for subsequent payloads
  /** SignalWire contexts, e.g. 'home', 'office'.. */
  "contexts": ["<context>", ...] // optional
}
```

```json
/**
 * JSONRPC Request
 */
{
  "jsonrpc": "2.0",
  "id": "<uuid>", // Or custom value
  ...RPC // per below
}
```

```json
/**
 * Example RPC Requests
 */
{
  "jsonrpc": "2.0",
  "id": "<uuid>",
  "method": "signalwire.connect",
  "params": {
    "agent": "<agent>",
    "version": {
      "major": 3,
      "minor": 0,
      "revision": 0
    },
    "authentication": {
      "project": "929863b4-a4a9-4904-9d77-6d7a91c38cff",
      "token": "PT7453a77e5a9afa7538c3a0d488851e3d47d3e97bcbd064bf"
    }
  }
}
```

```json
{
  "method": "signalwire.ping",
  "params": {
    "timestamp": 1666112809 // Unix timestamp
  }
}
```

```json
{
  "method": "calling.detect.digits",
  "params": {
    ...baseParams,
    "<key>": "<value>",
    ...
  }
}
```

```json
// "signalwire.reauthenticate"
```

```json
/**
 * JSONRPC Response
 */
{
  "id": "<id>", // Matching request
  "result": {
    "<key>": "<value>"
  }
}
```

```json
/**
 * Example RPC Responses
 */
{
  "id": "<id>", // From ping request
  "result": {
    "timestamp": 1666112809 // Unix timestamp
  }
}
```

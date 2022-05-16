# Hello World

This directory contains a package for a simple application.

## Library

The `hello-world` library is organized thusly:

* `HelloWorld.Contract`: Contains the REST schema.
* `HelloWorld.PAB`:
  * Contains the `HelloWorldContracts` contract, which turns our schema into a contract that can be run via the PAB.
  * `runPAB`: runs the PAB with our `HelloWorldContracts` contract, provided a "local cluster" is running.
* `HelloWorld.LocalCluster`:
  * `runCluster`: This runs a "local cluster" i.e. cardano-node(s) and our PAB.
  * `waitCluster`: Blocks until our PAB and wallet are confirmed to be up, or the timeout is reached.

## hello-world-cluster

This executable runs our local cluster. From there, one can step through a contract manually. This is primarily useful for testing or as a learning tool.

Below are instructions for this step-through. Our commands will use the `jq` tool to make the json output prettier, though you can leave this off if that is a problem.

<details>
  <summary>Click to expand instructions</summary>

1. First, start the cluster.

    ```
    $ cabal run hello-world-cluster
    ```

    You should see a bunch of output ending in:

    ```
    [pab:Info:1254] [2022-04-19 23:02:53.90 UTC] Restoring PAB state ...
    [pab:Info:1254] [2022-04-19 23:02:53.90 UTC] No contract instance were restored in the PAB state.
    [pab:Info:1254] [2022-04-19 23:02:53.90 UTC] Starting PAB backend server on port 9080
    ```

    You can further verify that everything is running normally:

    ```
    $ curl localhost:9080/api/healthcheck | jq
    []

    $ curl localhost:46493/v2/wallets | jq
    [
      {
        <long output omitted>
      }
    ]

    $ curl localhost:9080/api/contract/instances | jq
    []
    ```

2. Next, activate the contract.

    ```
    $ curl -H "Content-Type: application/json" -X POST -d @hello-world/test-client/activate.json localhost:9080/api/contract/activate | jq

    {
      "unContractInstanceId": "<contractId>"
    }
    ```

    The contract instance id will be unique every time. We will need that for the next steps.

    You should now see data with the following endpoints:

    ```
    $ curl localhost:9080/api/contract/instances | jq
    [
      {
        <long output omitted>
      }
    ]
    ```

    Additionally, we can query the status of our contract at any time.

    ```
    $ curl -H "Content-Type: application/json" localhost:9080/api/contract/instance/<contractId>/status | jq
    ```

3. Initialize

    ```
    $ curl -H "Content-Type: application/json" -X POST -d '[]' localhost:9080/api/contract/instance/<contractId>/endpoint/initialize | jq

    []
    ```

4. Increment

    ```
    $ curl -H "Content-Type: application/json" -X POST -d '[]' localhost:9080/api/contract/instance/<contractId>/endpoint/increment | jq

    []
    ```

5. Read

    ```
    $ curl -H "Content-Type: application/json" -X POST -d '[]' localhost:9080/api/contract/instance/<contractId>/endpoint/read | jq

    []
    ```

</details>

## hello-world-e2e

We can run an end-to-end test with

```
$ cabal run hello-world-e2e
```

This will essentially go through the above steps automatically, verifying that each step succeeds. Currently this is an `executable` rather than `test-suite` as this better fits into our CI. That is, our CI will run all `test-suite`s automatically with `cabal test`, and we do not necessarily want this.

## hello-world-unit

We can run the unit tests with

```
$ cabal run hello-world-unit
```

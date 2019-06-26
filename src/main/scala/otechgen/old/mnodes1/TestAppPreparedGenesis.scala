package otechgen.old.mnodes1

import otechgen.old._
import play.api.libs.json.Json

object TestAppPreparedGenesis1 extends App {

  val genesisStr =
    """
      {
        "header" : {
          "id" : 0,
          "version" : 1,
          "timestamp" : 1549201920684,
          "prev_block_hash" : "11111111111111111111111111111111",
          "signer_public_key" : "6xxYyW6QHy7Rx9eCcGuzNHsruvJ6VKc2Y8nWx1yJkscH",
          "signature" : "5HrFdJaUw9UEHPCC9W1peBde5buWAqrAi2KsQtf42zi2UPtUM5c28YdB7a4wtQRokR3jtGC2VQeMmEstDhfEfcAG",
          "hash" : "4PavH6CSCNNwhgHw1PyKQJqJbFynSDbRbxn4SHgNVzrz"
        }
      }
    """

  val genesis = Block.blockReads.reads(Json.parse(genesisStr)).get

  val account1 = Account.generate("I am account 1")
  val account2 = Account.generate("I am account 2")

  val nodeAddress1 = NodeAddress("127.0.0.1", 8080)
  val nodeAddress2 = NodeAddress("127.0.0.1", 8081)

  val fullNode1 = new Node("alpha", Some(account1), new MemoryStorage(genesis), nodeAddress1, false)
  fullNode1.storage.addNode(nodeAddress2)
  fullNode1.start

  Thread.sleep(3000)

  val fullNode2 = new Node("beta", Some(account1), new MemoryStorage(genesis), nodeAddress2, true)
  fullNode2.storage.addNode(nodeAddress1)
  fullNode2.start

}
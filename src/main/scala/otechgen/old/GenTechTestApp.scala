package otechgen.old

object GenTechTestApp extends App {

  println("App start")

  val genesisAccount = Account.generate("I am root")
  val genesis = Block.genesis(genesisAccount)

  println("Create node")

  val fullNode = new Node("dev", Some(genesisAccount), new MemoryStorage(genesis), NodeAddress("127.0.0.1", 8080), true)

  fullNode.start


}

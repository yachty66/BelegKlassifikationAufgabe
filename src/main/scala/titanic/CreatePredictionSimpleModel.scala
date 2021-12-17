package titanic

object CreatePredictionSimpleModel extends App{

  // load datasets
  val test = Utils.loadDataCSV("test.csv")
  println("Test Dataset:" + test.size + " Elements")

  val model= TitanicDataSet.simpleModel
  val prediction= NaiveBayes.applyModel(model,test,"passengerID")
  Utils.createSubmitFile("TitanicSimplePrediction.txt",prediction,"passengerID,survived")
  println(prediction)
}

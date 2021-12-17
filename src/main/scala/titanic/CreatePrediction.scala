package titanic

object CreatePrediction extends App{

  // load datasets
  val train = Utils.loadDataCSV("train.csv")
  val test = Utils.loadDataCSV("test.csv")

  println("Train Dataset:" + train.size + " Elements")
  println("Test Dataset:" + test.size + " Elements")

  val model= TitanicDataSet.createModelWithTitanicTrainingData(train,"survived")
  val evaluation= TitanicDataSet.createDataSetForTraining(test)
  val evalData= evaluation.map(map=>map-("survived"))
  val prediction= NaiveBayes.applyModel(model,evalData,"passengerID")
  Utils.createSubmitFile("TitanicPrediction.txt",prediction,"passengerID,survived")
  println(prediction)
}

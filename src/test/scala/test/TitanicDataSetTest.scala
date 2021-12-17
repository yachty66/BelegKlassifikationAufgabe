package test

import org.scalatest.funsuite.AnyFunSuite
import titanic._

class TitanicDataSetTest extends AnyFunSuite {

  // load datsets                    
  val train = Utils.loadDataCSV("train.csv")
  val test = Utils.loadDataCSV("test.csv")
  val all = train ++ test

  test("Test size of the datesets") {

    assert(train.size === 891)
    assert(test.size === 418)
  }

  test("Count missing values test") {
    
    val attList = List("passengerID", "pclass", "survived", "name", "sex", "age", "sibsp", "parch",
      "ticket", "fare", "cabin", "embarked")

    val train_mv = TitanicDataSet.countAllMissingValues(train, attList)
    val test_mv = TitanicDataSet.countAllMissingValues(test, attList)
    assert(train_mv("cabin") == 687 && train_mv("age") == 177 && train_mv("embarked") == 2)
    assert(test_mv("cabin") == 327 && test_mv("age") == 86 && test_mv("fare") == 1)
  }

  test("Extract Attributes for Training"){

    val res= TitanicDataSet.extractTrainingAttributes(train(5),List("passengerID","sex","age","survived","pclass"))
    val exp=Map("passengerID" -> 6, "sex" -> "male", "survived" -> 0, "pclass" -> 3)
    assert(res===exp)
  }

  test("create DataSet for Training"){

    val data= TitanicDataSet.createDataSetForTraining(train)
    val s=data.size
    val res= TitanicDataSet.countAllMissingValues(data,List("passengerID","sex","age","survived","pclass"))
    assert(s===891)
    assert(res===Map())
  }

  test("Create Model with Titanic Dataset"){

    val model= TitanicDataSet.createModelWithTitanicTrainingData(train, "survived")
    //age should be categorized
    val person1= Map[String,Any]("passengerID" -> 6, "sex" -> "male", "pclass" -> 3,"age"->60)
    val person2= Map[String,Any]("passengerID" -> 23, "sex" -> "female", "pclass" -> 1,"age"->3)
    val predict1= model(person1,"passengerID")
    val predict2= model(person2,"passengerID")
    println("Hier wird für 2 fiktive Personen eine Vorhersage mit dem erzeugten Modell getroffen. Dazu muss das" +
      "Attribut entsprechend der gewählten Skala angepasst werden (entsprechend des vorgegebenen Alters." +
      "Zu erwarten ist, dass die die erste Person nicht überlebt und die zweite überlebt.")
    println("predict1: "+predict1)
    println("predict2: "+predict2)
  }
}
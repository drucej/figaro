package dscof

import com.cra.figaro.algorithm.AutomaticDifferentiation.AutoDiffVariableElimination
import com.cra.figaro.language.{Flip, _}
import com.cra.figaro.library.compound.If
import com.cra.figaro.language.DualElement

object test {


  val universe = Universe.universe

  //  val test1 = Constant(0.4);
  //  val param = Constant(0.7)
  //  val test2 = If(Flip(param), Constant(1), Constant(2))
  //

  //val pThreat = (Constant(0.25)("pThreat",universe)).asInstanceOf[DualElement];
  val pThreat =  Constant(0.25)("pThreat",universe)

  val pIndGivenThreat = DualConstant(0.9)("pIndGivenThreat",universe);
  val pIndGivenNotThreat = Constant(0.15)("pIndGivenNotThreat",universe);

  val Threat = Flip(pThreat)("Threat",universe);
  val Ind = Flip(If(Threat,pIndGivenThreat,pIndGivenNotThreat))("Ind",universe);

  def main(args: Array[String]) {

    val alg1 = AutoDiffVariableElimination(List(pIndGivenThreat),Ind)
    alg1.start()
    alg1.kill
    //
    //  val alg2 = VariableElimination(Ind)
    //  alg2.start()
    //  alg2.kill

  }
}
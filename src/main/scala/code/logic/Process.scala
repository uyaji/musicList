package code.logic
import code.model._
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import SHtml._
import net.liftweb.util._

object Process {
  def add(function1: ((Target, Binder) => Boolean) => (Target, Relation, Binder, String) => List[FieldError], function2: (Target, Binder) => Boolean, function3: String => List[Target], key: String, binder: Binder, generatedTarget: Target, generatedRelation: Relation, largeObject: Option[LargeObject], msg: String, errMsg: String, path: String) {
    val target = function3(key) match {
      case Nil => generatedTarget
      case targets: List[Target] => targets.head
    }
    largeObject match {
      case Some(lo) => target.setLob(lo.asInstanceOf[target.SuitableObject])
      case None => ()
    }
    function1(function2)(target, generatedRelation, binder, errMsg) match {
      case Nil => {
        target.save
        generatedRelation match {
          case null => ()
          case _ => {
            generatedRelation.setTarget(target.getId)
            generatedRelation.save
          }
        }
        S.notice(msg)
        S.redirectTo(path)
      }
      case errors: List[FieldError] => {
        errors(0).field match {
          case null => S.error(errors(0).msg)
          case _ => S.error(errors)
        }
        S.redirectTo(path)
      }
      case _ => ()
    }
  }
}

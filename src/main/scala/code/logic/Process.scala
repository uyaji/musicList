package code.logic
import code.model._
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import SHtml._
import net.liftweb.util._

object Process {
  def add(function1: ((Target, Binder) => Boolean) => (Target, Relation, Binder, String) => List[FieldError], function2: (Target, Binder) => Boolean, function3: String => List[Target], key: String, binder: Binder, generatedTarget: Target, generatedRelation: Relation, largeObject: Option[LargeObject], seq: Int, msg: String, errMsg: String, path: String) {
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
          case null => {
            key match {
              case "" => ()
              case _ => {
                binder.save
                target.setTarget(binder.getTargets.filter{bsq => bsq.getSeq == seq}.head.getId)
                target.save
              }
            }
          }
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

  def update(function1: ((Long, Long) => Target, Long => Binder, String => List[Target]) => (Long, Long, String) => Result, function2: (Long, Long) => Target, function3: Long => Binder, function4: String => List[Target], key: String, seq: Long, bandSeq: Binder, relationId: Long, msg: String, errorMsg: String, path: String) {
    val target = function2(bandSeq.getId, relationId)
    val relation = target.getRelation(relationId)
    val existTargets = function4(key)
    val result = function1(function2, function3, function4)(bandSeq.getId, relationId, key)
    result.error match {
      case true => {
        S.error(errorMsg)
        S.redirectTo(path)
      }
      case false => ()
    }
    result.changeContent match {
      case "name" => {
        target.setName(key)
      }
      case _ => {
        relation.setTarget(existTargets.head.getId)
      }
    }
    relation.setSeq(seq)
    relation.save
    target.save
    S.notice(msg)
    S.redirectTo(path)
  }
}

package code.logic
import code.model._
import net.liftweb.mapper._
import net.liftweb.http._
import S._
import SHtml._
import net.liftweb.util._
import net.liftweb.common._

object Process {
  def add(function1: ((Target, Binder) => Boolean) => (Target, Relation, Binder, String) => List[FieldError], function2: (Target, Binder) => Boolean, function3: String => List[Target], key: String, binder: Binder, generatedTarget: Target, generatedRelation: Relation, largeObject: Option[LargeObject], seq: Int, msg: String, errMsg: String): scala.xml.NodeSeq = {
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
        scala.xml.XML.loadString("<li>" + msg + "</li>")
      }
      case errors: List[FieldError] => {
        errors(0).field match {
          case null => errors(0).msg
          case _ => errors(0).msg
        }
      }
      case _ => scala.xml.XML.loadString("<li></li>")
    }
  }

  def update(function1: ((Long, Long) => Target, Long => Binder, String => List[Target]) => (Long, Long, String) => Result, function2: (Long, Long) => Target, function3: Long => Binder, function4: String => List[Target], function5: Box[FileParamHolder] => Boolean, function6: String => List[LargeObject], key: String, seq: Long, upload: Box[FileParamHolder], binder: Binder, relationId: Long, generateLargeObject: Option[LargeObject], msg: String, errorMsgTarget: String, errorMsgLargeObject: String): scala.xml.NodeSeq = {
    val largeObject = function5(upload) match {
      case true => function6(generateLargeObject.get.getFileName) match {
        case Nil => generateLargeObject.get
        case largeObjects: List[LargeObject] => largeObjects.head
      }
      case false => null
    }
    val target = function2(binder.getId, relationId)
    val relation = target.getRelation(relationId)
    val existTargets = function4(key)
    val result = function1(function2, function3, function4)(binder.getId, relationId, key)
    result.error match {
      case true => {
        return scala.xml.XML.loadString("<li>" + errorMsgTarget + "</li>")
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
    if(function5(upload)) {
      target.getLobs.contains(largeObject) match {
      // 重複エラー
        case true => {
          return scala.xml.XML.loadString("<li>" + errorMsgLargeObject + "</li>")
        }
        case false => {
          target.setLob(largeObject.asInstanceOf[target.SuitableObject])
        }
      }
    }
    relation.setSeq(seq)
    relation.save
    target.save
    scala.xml.XML.loadString("<li>" + msg + "</li>")
  }
}

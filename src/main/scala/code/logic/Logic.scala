package code.logic

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util._
import S._
import code.model.Target
import code.model.Binder
import code.model.Relation
import code.model.AlbumTracks
import code.model.LargeObject
import net.liftweb.util.FieldError

object Logic {
  def select(duplicateCheck: (Long, Long) => Boolean, changeKeyCheck: (Long, Long) => Boolean)(target: Long, key: Long, relationKey: Long, msg: String, path: String): String = {
    duplicateCheck(target, key) match {
      // seqの重複なし。
      case true => {
        relationKey match {
          // 新規登録
          case 0 => "add"
          // 既存データ表示からの変更登録。
          case _ => "update"
        }
      }
      // seqの重複あり。
      case _ => {
        // seqの変更が無ければ、更新。
        relationKey match {
          // 新規登録からの既存SEQへの変更。
          case 0 => {
            S.error(msg)
            S.redirectTo(path)
          }
          // 既存データ表示からの変更登録。
          case _ => {
            changeKeyCheck(relationKey, key) match {
              // seqの変更が無ければ、更新。
              case true => "update"
              case _ => {
                S.error(msg)
                S.redirectTo(path)
              }
            }
          }
        }
      }
    }
  }

  def updateTarget(getTarget: (Long, Long) => Target, getBinder: Long => Binder, getExistTarget: String => List[Target])(binderId: Long, relationId: Long, name: String, errMsg: String): Result = {
    val result = new Result(Nil, "")
    val target = getTarget(binderId, relationId)
    // 指定されたtargetが既存かどうかチェック。
    //   既存: relationのアソシエーションの変更
    //   未存: targetのnameの更新
    var exist = false
    // 入力されたnameで、targetオブジェクトをインスタンス化
    val existTargets = getExistTarget(name)
    // nameの変更を確認
    target.getName.equals(name) match {
      // 変更が無ければ、重複問題なし
      case true => exist = true
      // 変更の場合、重複の可能性あり
      case _ => {
        // 入力targetオブジェクトの有無確認
        existTargets.size match {
          case 0 => exist = true
          case _ => exist = false
        }
      }
    }
    exist match {
      // 重複の問題がないので、nameの変更。
      case true => {
        // targetのname変更
        result.changeContent="name"
      }
      // 重複の恐れあり。binder内のtargetをチェック
      case _ => {
        if(target.isInstanceOf[code.model.Album]) {
/*          binder.getTargets.forall{ bsq => !bsq.getTarget2s.contains(existTargets.head)} match {
            case false => {*/
              result.errors = List(FieldError(null, <li>{errMsg}</li>))
/*            }
            // 重複がないので、アソシエーションの変更。
            case true => {
              result.changeContent="relation"
            }
          }*/
        } else {
          val binder = getBinder(binderId)
          binder.getTargets.contains(existTargets.head) match {
            // 重複あり
            case true => {
              result.errors = List(FieldError(null, <li>{errMsg}</li>))
            }
            // 重複がないので、アソシエーションの変更。
            case false => {
              result.changeContent="relation"
            }
          }
        }
      }
    }
    result
  }

  def registTarget(duplicateKeyCheck: (Target, Binder) => Boolean)(target: Target, generatedRelation: Relation, binder: Binder, errMsg: String): List[FieldError] = {
    // 登録時にターゲットの重複がないかチェック
    duplicateKeyCheck(target, binder) match {
      case true => {
        List(FieldError(null, <li>{errMsg}</li>))
      }
      case false => Nil
    }
  }
}

case class Result (
  var errors: List[FieldError],
  var changeContent: String
)

package code.logic

import net.liftweb.common._
import net.liftweb.http._
import S._
import code.model.Target
import code.model.Binder
import code.model.Relation
import code.model.Attach

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

  def updateTarget(getTarget: (Long, Long) => Target, getBinder: Long => Binder, getRelation: Long => Relation, getExistTarget: String => List[Target], isAtachFileExist: Box[FileParamHolder] => Boolean)(binderId: Long, relationId: Long, name: String, path: String, msg: String, errorMsg: String, seq: Long, upload: Box[FileParamHolder], attach: Attach): Unit = {
    val target = getTarget(binderId, relationId)
    val relation = getRelation(relationId)
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
        target.setName(name)
      }
      // 重複の恐れあり。BandSeq内のplayerをチェック
      case _ => {
        val binder = getBinder(binderId)
        binder.getTargets.contains(existTargets.head) match {
          // 重複あり
          case true => {
            S.error(errorMsg)
            S.redirectTo(path)
          }
          // 重複がないので、アソシエーションの変更。
          case false =>
            relation.setTarget(existTargets.head.getId)
        }
      }
    }
    if(isAtachFileExist(upload)) {
      target.setLob(attach)  
    }
    target.save
    relation.setSeq(seq)
    relation.save
    S.notice(msg)
    S.redirectTo(path)
  }
}


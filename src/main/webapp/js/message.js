var page;
$('#message').hover(function () {
  page = 0;
  next(1);
}
);

function modalClose() {
  $(".modalClose").parents(".modal").fadeOut();
  $(".modalOpen").removeClass("open");
}

function next(inc) {
  page = page + inc
  $.ajax({
    url : "selectTwiters/" + $('#user').val(),
    dataType: "json",
    success: function( datas ) {
      $(".inner").empty();
      var cnt = 0;
      datas.forEach( function( data ) {
        if(cnt >= (page -1) * 5 && cnt < page * 5) {
          $(".inner").append("<a href =twit?to=" + data.value + ">"+ data.label + "</a><br/>");
        } 
        cnt = cnt + 1;
      });
      $(".inner").append("<br /><a href='' onClick='modalClose();return false;'>exit</a>");
      $(".inner").append("<span>&nbsp&nbsp</span>");
      if(page > 1) {
        $(".inner").append("<a href='' onClick='next(-1);return false;'>prev</a>");
        $(".inner").append("<span>&nbsp&nbsp</span>");
      }
      if(datas.length > page * 5) {
        $(".inner").append("<a href='' onClick='next(1);return false;'>next</a>");
        $(".inner").append("<span>&nbsp&nbsp</span>");
      }
      $(".inner").append("<span> " + page + " page</span>");
      var navClass = $(".modalOpen").attr("class"),
      href = $(".modalOpen").attr("href");
            
      $(href).fadeIn();
      $(".modalOpen").addClass("open");
      return false;
    }
  });
  return false;
}

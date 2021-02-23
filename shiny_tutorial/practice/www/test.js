$(function() {
  Shiny.addCustomMessageHandler('add-remove-cards', function(message) {
    
    var card = $.parseHTML(message.card);
    var btn  = message.add_remove
    var card_len = $(".my-cards").length;
    
    if(btn === "add") {
      if(card_len === 0) {
        $(card).insertAfter($('#placeholder'));
      } else {
        $(card).insertAfter($('.my-cards:last'));
      }
    } else {
      $(".my-cards").last().remove()
    }
  });
});
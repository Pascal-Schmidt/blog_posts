$("button").click(function() {
    var id = this.id;
    Shiny.setInputValue("button_clicked", id, {priority: "event"})
});
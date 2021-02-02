function get_id(clicked_id) {
     Shiny.setInputValue("current_id", clicked_id, {priority: "event"});
}
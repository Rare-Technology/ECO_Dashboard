getDownloadLink = function(id) {
  download_link_div = $("#download-report-link-div");
  download_link_div.innerText = $("#" + id).attr("href");
}

disableDownloadLink = function(id) {
  $("#" + id).attr("href", "javascript: void(0)");
}

enableDownloadLink = function(id) {
  download_link = $("#download-report-link-div").attr("href");
  $("#" + id).attr("href", download_link);
}
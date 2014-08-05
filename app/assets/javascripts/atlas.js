function logOut() {
    ajaxRequest("/api/sessions", "DELETE", null, null, redirect);
}

function redirect(url) {
window.location.href = url.url;
}

function ajaxRequest(url, type, cType, data, funct) {
    var contentType = (cType !== null) ? cType : "application/x-www-login-urlencoded";
    $.ajax({
        url: url,
        type: type,
        contentType: contentType + '; charset=UTF-8',
        data: JSON.stringify(data),
        success: function (message) {
            funct(message);
        },
        error: function (error) {
            console.log(error);
        }
    });
}
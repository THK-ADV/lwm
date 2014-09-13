function logOut() {
    ajaxRequest("/api/sessions", "DELETE", null, null, redirect);
}

function redirect(url) {
window.location.href = url.url;
}

function reload(data) {
window.location.reload(true);
}

function deleteStudent(id) {
    ajaxRequest("/administration/students", "DELETE", "application/json", {id: id}, reload);
}

function deleteUser(id) {
    ajaxRequest("/administration/users", "DELETE", "application/json", {id: id}, reload);
}

function deleteCourse(id) {
    ajaxRequest("/administration/courses", "DELETE", "application/json", {id: id}, reload);
}

function deleteDegree(id) {
    ajaxRequest("/administration/degrees", "DELETE", "application/json", {id: id}, reload);
}


function deleteLabwork(id) {
    ajaxRequest("/administration/labworks", "DELETE", "application/json", {id: id}, reload);
}

function deleteRoom(id) {
    ajaxRequest("/administration/rooms", "DELETE", "application/json", {id: id}, reload);
}

function deleteTableEntry(entry, labwork) {
    ajaxRequest("/administration/timetable", "DELETE", "application/json", {eId: entry, lId: labwork}, reload);
}

function deleteSemester(id) {
    ajaxRequest("/administration/semesters", "DELETE", "application/json", {id: id}, reload);
}

function deleteAssignment(id) {
    ajaxRequest("/administration/assignments", "DELETE", "application/json", {id: id}, reload);
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
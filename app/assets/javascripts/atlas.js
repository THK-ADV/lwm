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

function deleteBlacklist(id) {
    ajaxRequest("/administration/blacklist", "DELETE", "application/json", {id: id}, reload);
}

function deleteBlacklistDate(listId, dateId) {
    ajaxRequest("/administration/blacklist/dates", "DELETE", "application/json", {listId: listId, dateId : dateId}, reload);
}

function deleteBinding(labid, aid) {
    var l = encodeURIComponent(labid);
    var a = encodeURIComponent(aid);
    ajaxRequest("/administration/labworks/"+l+"/associations/"+a, "DELETE", "application/json", {lId: labid, aId: aid}, reload);
}

function addStudentToGroup(labid, groupid) {
    var student = $('#autocomplete').val();
    var e = encodeURIComponent(labid);
    ajaxRequest("/administration/labworks/"+e, "POST", "application/json", {student: student,group: groupid}, reload);
}

function removeStudentFromGroup(labid, student, groupid) {
    var e = encodeURIComponent(labid);
    ajaxRequest("/administration/labworks/"+e, "DELETE", "application/json", {student: student, group: groupid}, reload);
}


function setPreparationTime(labid, gid, aid) {
    var newTime = $('#selectPreparationTime').find(":selected").text().split("+")[1];
    var e = encodeURIComponent(labid);
    var a = encodeURIComponent(aid);
    ajaxRequest("/administration/labworks/"+e+"/associations/"+a, "PUT", "application/json", {time: newTime, group: gid}, reload);
}

function createSchedule(uri, id) {
    ajaxRequest(uri, 'POST', 'application/json', {id: id}, reload);
}

function affirm() {
    return confirm("Sind sie sicher?");
}

function setVisible(uri, labid, visible) {
    ajaxRequest(uri, 'PUT', 'application/json', {id: labid, visibility: visible}, reload);
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
            alert("Shit");
            console.log(error);
        }
    });
}
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

function deleteTableEntry(entry, timetable, labwork) {
    var tu = encodeURIComponent(timetable);
    ajaxRequest("/administration/timetable/" + tu + "/entries", "DELETE", "application/json", {eId: entry, lId: labwork}, reload);
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
    ajaxRequest("/administration/blacklist/dates", "DELETE", "application/json", {listId: listId, dateId: dateId}, reload);
}

function deleteBinding(labid, aid) {
    var l = encodeURIComponent(labid);
    var a = encodeURIComponent(aid);
    ajaxRequest("/administration/labworks/" + l + "/associations/" + a, "DELETE", "application/json", {lId: labid, aId: aid}, reload);
}

function addStudentToGroup(labid, groupid) {
    var student = $('#autocomplete').val();
    var e = encodeURIComponent(labid);
    ajaxRequest("/administration/labworks/" + e, "POST", "application/json", {student: student, group: groupid}, reload);
}

function removeStudentFromGroup(labid, student, groupid) {
    var e = encodeURIComponent(labid);
    ajaxRequest("/administration/labworks/" + e, "DELETE", "application/json", {student: student, group: groupid}, reload);
}

function createSchedule(uri, id) {
    ajaxRequest(uri, 'POST', 'application/json', {id: id}, reload);
}

function confirmed() {
    return confirm("Sind sie sicher?");
}

function setVisible(uri, labid, visible) {
    ajaxRequest(uri, 'PUT', 'application/json', {id: labid, visibility: visible}, reload);
}

function applyEdit(q, uri) {
    ajaxRequest("/administration/edits", "POST", "application/json", {query: q, redirect: uri}, reload);
}

function groupList(uri) {
    var min = $("#min").val();
    var max = $("#max").val();
    if(min !== '' && max !== '') {
        ajaxRequest("/administration/labworkApplications/" + encodeURIComponent(uri), "POST", "application/json", {min: min, max: max}, reload);
    }
}

function deleteApplication(applicationId, listId) {
    ajaxRequest("/administration/labworkApplications", "DELETE", "application/json", {app: applicationId, list: listId}, reload);
}

function sDeleteApplication(studentId, labworkid) {
    ajaxRequest("/students/labworkApplications", "DELETE", "application/json", {s: studentId, lab: labworkid}, reload);
}

function createStudentApplication(listId) {
    var s = $("#autocomplete").val();
    ajaxRequest('/administration/labworkApplicationLists/' + encodeURIComponent(listId), "POST", "application/json", {student: s}, reload);
}

function search() {
    var uri = $("#autocomplete").val();
    redirect({url: "/administration/students/search/" + encodeURIComponent(uri)});
}

function postAttendance(association) {
    alert(association);
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

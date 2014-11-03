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


function displayUserData(data) {
    $('#userData').html(data);
}

function currentUser() {
    $(document).ready(function() {
        ajaxRequest("/currentUser", "POST", "application/json", {}  , displayUserData);
    });
}

function displayBreadCrumbs(data) {
    $("#breadcrumbs").html(data);
}

function breadCrumbs(label) {
    var path = window.location.pathname;
    if(path.indexOf("/administration/students/page") > -1) path = "";
    ajaxRequest("/breadcrumbs", "POST", "application/json", {label: label, url: path}, displayBreadCrumbs);
}

function swapGroups(index, student, oldGroup) {
    var newGroup = $("#groupSwap"+index).val();
    ajaxRequest("/administration/groups/swap", "POST", "application/json", {student: student, old: oldGroup, new: newGroup}, reload);
}

function removeAlternateDate(student, schedule) {
    ajaxRequest("/students/overview/"+encodeURIComponent(schedule), "DELETE", "application/json", {student: student}, reload);
}

var timer = 5;

var socket;
var socketUrl = "";
var interval = null;

function createSocket(url) {
    socketUrl = url;
    socket = new WebSocket(url);
    socket.onmessage = function(event){
        var json = JSON.parse(event.data);
        if(json.type != "ping"){
            var id = json.id;
            var status = json.status;
            $(id).prop("checked", status);
        }else{
            $( "#connectionWarning" ).hide();
            timer = 5;
            if(interval === null){
                startTimer();
            }
        }
    };
}

function createUserCountSocket(url) {
    socketUrl = url;
    socket = new WebSocket(url);
    socket.onmessage = function(event){
        var json = JSON.parse(event.data);

        if(json.type == "studentCount"){

            if(json.count === 0){
                $("#studentCount").html("");
                $("#studentenText").html("Kein Student");
            }else if(json.count === 1){
                $("#studentCount").html("");
                $("#studentenText").html("Ein Student");
            }else{
                $("#studentCount").html(json.count);
                $("#studentenText").html("Studenten");
            }
        }else if(json.type == "userCount"){
            if(json.count === 0){
                $("#userCount").html("");
                $("#userText").html("Kein Mitarbeiter");
            }else if(json.count === 1){
                $("#userCount").html("");
                $("#userText").html("Ein Mitarbeiter");
            }else{
                $("#userCount").html(json.count);
                $("#userText").html("Mitarbeiter");
            }
        }
    };
}

function connectionTimer() {
    timer = timer -1;
    if(timer < 1){
        clearInterval(interval);
        $( "#connectionWarning" ).show( "highlight", {}, 500, {});
        $( "#saveButton" ).show( "highlight", {}, 500, {});
    }
}

function startTimer(){
    interval = setInterval(connectionTimer, 2000);
}


var localState = {};
var keys = [];
var dirty = false;

function initLocalState(association, attended, passed) {
    keys.push(association);
    localState[association] = {
        "association" : association,
        "attended" : attended,
        "passed" : passed
    };
}

function attendanceSwitch(association) {
    localState[association].attended = !localState[association].attended;
    dirty = true;

    var data = {
        "type" : "attendance-change",
        "association" : association
    };
    $( "#saveButton" ).show( "highlight", {}, 500, {});
    socket.send(JSON.stringify(data));
}

function passedSwitch(association) {
    localState[association].passed = !localState[association].passed;
    dirty = true;

    var data = {
        "type" : "passed-change",
        "association" : association
    };
    $( "#saveButton" ).show( "highlight", {}, 500, {});
    socket.send(JSON.stringify(data));
}

function postSupervisionChanges(url){
    dirty = false;
    var temp = [];

    keys.forEach(function(entry) {
        temp.push(localState[entry]);
    });

    var postData = {
        "keys" : keys,
        "data" : temp
    };

    $("#connectionWarning").hide();
    $("#saveButton").hide();

    ajaxRequest(url, "POST", "application/json",postData, {});
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

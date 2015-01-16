var hideStates = {};

function initHideState(studentUri, status){
    hideStates[studentUri] = {
        hidden: status
    };
}

function toggleHiddenStatus(labworkUri, studentUri, studentHash){

    ajaxHO("/administration/hideStates", "POST", "application/json", {student: studentUri, labwork: labworkUri, hide: !hideStates[studentUri].hidden}, function(data){
        if(data.hidden){
            hideStates[studentUri].hidden = true;
            $("#eye" + studentHash).removeClass().addClass("glyphicon glyphicon-eye-close");
            $("#" + studentHash).addClass("danger");
        }else{
            hideStates[studentUri].hidden = false;
            $("#eye" + studentHash).removeClass().addClass("glyphicon glyphicon-eye-open");
            $("#" + studentHash).removeClass();
        }
    } );
}

function logOut() {
    ajaxHO("/api/sessions", "DELETE", null, null, redirect);
}

function redirect(url) {
    window.location.href = url.url;
}

function reload(data) {
    window.location.reload(true);
}

function deleteStudent(id) {
    ajax("/administration/students", "DELETE", "application/json", {id: id});
}

function deleteUser(id) {
    ajax("/administration/users", "DELETE", "application/json", {id: id});
}

function deleteCourse(id) {
    ajax("/administration/courses", "DELETE", "application/json", {id: id});
}

function deleteDegree(id) {
    ajax("/administration/degrees", "DELETE", "application/json", {id: id});
}


function deleteLabwork(id) {
    ajax("/administration/labworks", "DELETE", "application/json", {id: id});
}

function deleteRoom(id) {
    ajax("/administration/rooms", "DELETE", "application/json", {id: id});
}

function deleteTableEntry(entry, timetable, labwork) {
    var tu = encodeURIComponent(timetable);
    ajax("/administration/timetable/" + tu + "/entries", "DELETE", "application/json", {eId: entry, lId: labwork});
}

function deleteSemester(id) {
    ajax("/administration/semesters", "DELETE", "application/json", {id: id});
}

function deleteAssignment(id) {
    ajax("/administration/assignments", "DELETE", "application/json", {id: id});
}

function deleteBlacklist(id) {
    ajax("/administration/blacklist", "DELETE", "application/json", {id: id});
}

function deleteBlacklistDate(listId, dateId) {
    ajax("/administration/blacklist/dates", "DELETE", "application/json", {listId: listId, dateId: dateId});
}

function deleteLiveAssignment(assignmentId) {
    ajax("/administration/live", "DELETE", "application/json", {id: assignmentId});
}

function deleteBinding(labid, aid) {
    var l = encodeURIComponent(labid);
    var a = encodeURIComponent(aid);
    ajax("/administration/labworks/" + l + "/associations/" + a, "DELETE", "application/json", {lId: labid, aId: aid});
}

function addStudentToGroup(labid, groupid) {
    var student = $('#autocomplete').val();
    var e = encodeURIComponent(labid);
    ajax("/administration/labworks/" + e, "POST", "application/json", {student: student, group: groupid});
}

function removeStudentFromGroup(labid, student, groupid) {
    var e = encodeURIComponent(labid);
    ajax("/administration/labworks/" + e, "DELETE", "application/json", {student: student, group: groupid});
}

function createSchedule(uri, id) {
    ajax(uri, 'POST', 'application/json', {id: id});
}

function confirmed() {
    return confirm("Sind sie sicher?");
}

function setVisible(uri, labid, visible) {
    ajax(uri, 'PUT', 'application/json', {id: labid, visibility: visible});
}

function setApproval(labwork, student) {
    ajaxHO("/administration/labworks/" + encodeURIComponent(labwork) + "/approvals", "POST", "application/json", {student: student}, function(data){
        if(data.approvedState) {
            $("#"+data.id).removeClass().addClass("bg-success");
            $("#approval"+data.id).removeClass().addClass("glyphicon glyphicon-check");
        } else {
            $("#"+data.id).removeClass().addClass("bg-danger");
            $("#approval"+data.id).removeClass().addClass("glyphicon glyphicon-fire");
        }
    });
}


function applyEdit(q, uri) {
    ajax("/administration/edits", "POST", "application/json", {query: q, redirect: uri});
}

function groupList(uri) {
    var min = $("#min").val();
    var max = $("#max").val();
    if(min !== '' && max !== '') {
        ajax("/administration/labworkApplications/" + encodeURIComponent(uri), "POST", "application/json", {min: min, max: max});
    }
}

function deleteApplication(applicationId, listId) {
    ajax("/administration/labworkApplications", "DELETE", "application/json", {app: applicationId, list: listId});
}

function sDeleteApplication(studentId, labworkid) {
    ajax("/students/labworkApplications", "DELETE", "application/json", {s: studentId, lab: labworkid});
}

function createStudentApplication(listId) {
    var s = $("#autocomplete").val();
    ajax('/administration/labworkApplicationLists/' + encodeURIComponent(listId), "POST", "application/json", {student: s});
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
        ajaxHO("/currentUser", "POST", "application/json", {}  , displayUserData);
    });
}

function displayBreadCrumbs(data) {
    $("#breadcrumbs").html(data);
}

function breadCrumbs(label) {
    var path = window.location.pathname;
    ajaxHO("/breadcrumbs", "POST", "application/json", {label: label, url: path}, displayBreadCrumbs);
}

function swapGroups(index, student, oldGroup) {
    var newGroup = $("#groupSwap"+index).val();
    ajax("/administration/groups/swap", "POST", "application/json", {student: student, old: oldGroup, new: newGroup});
}

function removeAlternateDate(student, oldSchedule, schedule, hash) {
    $("#new" + hash).remove();
    $("#rm" + hash).remove();
    $("#old" + hash).removeClass().addClass("text-success");
    ajaxHO("/students/overview/"+encodeURIComponent(schedule), "DELETE", "application/json", {student: student, schedule: schedule, oldSchedule: oldSchedule}, function(data){
        alert(JSON.stringify(data));
    });

}

function postAlternateDate(student, oldSchedule, hash){
    var schedule = $("#newSchedule" + hash + " option:selected").val();
    ajaxHO("/students/overview/"+encodeURIComponent(oldSchedule), "POST", "application/json", {student: student, schedule: schedule, oldSchedule: oldSchedule}, function(data){
        $("#dates" + hash).append("<br /> <span>" + $("#newSchedule" + hash + " option:selected").text() + "</span>");
        $("#" + hash).modal('hide');
    });
}

function removeStatement(resource, property, rdfnode) {
    ajax("/superuser", "DELETE", "application/json", {resource: resource, property: property, node: rdfnode});
}

function completeSearch() {
    var parameter = $("#completeSearch").val();
    redirect({url: "/administration/search?param="+parameter});
}

function resourceFormCatcher(hash, url) {
        var prevRes = $("#prevRes"+hash).val();
        var prevProp = $("#prevProp"+hash).val();
        var prevNode = $("#prevNode"+hash).val();
        var nextRes = $("#nextRes"+hash).val();
        var nextProp = $("#nextProp"+hash).val();
        var nextNode = $("#nextNode"+hash).val();

        nextRes = (prevRes != nextRes) ? nextRes : prevRes;
        nextProp = (prevProp != nextProp) ? nextProp : prevProp;
        nextNode = (prevNode != nextNode) ? nextNode : prevNode;

        applyEdit("UPDATE "+nextProp+" WITH "+nextNode+" IN "+nextRes, url);
}

function superUserUpdate(hash, url) {

    var obj = {
        prevRes: $("#prevRes"+hash).val(),
        prevProp: $("#prevProp"+hash).val(),
        prevNode: $("#prevNode"+hash).val(),
        nextRes:  $("#nextRes"+hash).val(),
        nextProp: $("#nextProp"+hash).val(),
        nextNode: $("#nextNode"+hash).val(),
        url: url
    };

    ajax("/superuser", "POST", "application/json", obj);
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
        "ss_attended": attended,
        "ss_passed": passed,
        "attended" : attended,
        "passed" : passed
    };
}

function attendanceSwitch(association, id, user) {
    localState[association].attended = !localState[association].attended;

    if(localState[association].passed == localState[association].ss_passed && localState[association].attended == localState[association].ss_attended){
        localState[association].dirty = false;
    }else{
        dirty = true;
        localState[association].dirty = true;
    }
    var data = {
        "type" : "attendance-change",
        "user" : user,
        "association" : association
    };
    $( "#saveButton" ).show( "highlight", {}, 500, {});

    if(localState[association].attended){
        $("#" + id).removeClass("text-danger").addClass("text-success");
    }else{
        $("#" + id).removeClass("text-success").addClass("text-danger");
    }

    socket.send(JSON.stringify(data));
}

function passedSwitch(association, id, user) {
    localState[association].passed = !localState[association].passed;

    if(localState[association].passed == localState[association].ss_passed && localState[association].attended == localState[association].ss_attended){
        localState[association].dirty = false;
    }else{
        dirty = true;
        localState[association].dirty = true;
    }



    var data = {
        "type" : "passed-change",
        "user" : user,
        "association" : association
    };
    $( "#saveButton" ).show( "highlight", {}, 500, {});

    if(localState[association].passed){
        $("#" + id).removeClass("glyphicon glyphicon-ban-circle text-danger").addClass("glyphicon glyphicon-ok text-success");
    }else{
        $("#" + id).removeClass("glyphicon glyphicon-ok text-success").addClass("glyphicon glyphicon-ban-circle text-danger");
    }
    socket.send(JSON.stringify(data));
}

function postSupervisionChanges(url){
    dirty = false;
    var temp = [];

    keys.forEach(function(entry) {
        if(localState[entry].dirty){
            temp.push(localState[entry]);
        }
    });

    var postData = {
        "data" : temp
    };

    $("#connectionWarning").hide();
    $("#saveButton").hide();



    ajax(url, "POST", "application/json",postData);
}

function ajaxHO(url, type, cType, data, funct) {
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
            console.log(JSON.stringify(error));
        }
    });
}

function ajax(url, type, cType, data) {
    var contentType = (cType !== null) ? cType : "application/x-www-login-urlencoded";
    $.ajax({
        url: url,
        type: type,
        contentType: contentType + '; charset=UTF-8',
        data: JSON.stringify(data),
        error: function (error) {
            console.log(JSON.stringify(error));
        }
    });
}
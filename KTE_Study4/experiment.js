var BrowserDetect = {
  init: function () {
    this.browser = this.searchString(this.dataBrowser) || "An unknown browser";
    this.version = this.searchVersion(navigator.userAgent)
      || this.searchVersion(navigator.appVersion)
      || "an unknown version";
    this.OS = this.searchString(this.dataOS) || "an unknown OS";
  },
  searchString: function (data) {
    for (var i=0;i<data.length;i++) {
      var dataString = data[i].string;
      var dataProp = data[i].prop;
      this.versionSearchString = data[i].versionSearch || data[i].identity;
      if (dataString) {
        if (dataString.indexOf(data[i].subString) != -1)
          return data[i].identity;
      }
      else if (dataProp)
        return data[i].identity;
    }
  },
  searchVersion: function (dataString) {
    var index = dataString.indexOf(this.versionSearchString);
    if (index == -1) return;
    return parseFloat(dataString.substring(index+this.versionSearchString.length+1));
  },
  dataBrowser: [
    {
      string: navigator.userAgent,
      subString: "Chrome",
      identity: "Chrome"
    },
    {   string: navigator.userAgent,
      subString: "OmniWeb",
      versionSearch: "OmniWeb/",
      identity: "OmniWeb"
    },
    {
      string: navigator.vendor,
      subString: "Apple",
      identity: "Safari",
      versionSearch: "Version"
    },
    {
      prop: window.opera,
      identity: "Opera",
      versionSearch: "Version"
    },
    {
      string: navigator.vendor,
      subString: "iCab",
      identity: "iCab"
    },
    {
      string: navigator.vendor,
      subString: "KDE",
      identity: "Konqueror"
    },
    {
      string: navigator.userAgent,
      subString: "Firefox",
      identity: "Firefox"
    },
    {
      string: navigator.vendor,
      subString: "Camino",
      identity: "Camino"
    },
    {   // for newer Netscapes (6+)
      string: navigator.userAgent,
      subString: "Netscape",
      identity: "Netscape"
    },
    {
      string: navigator.userAgent,
      subString: "MSIE",
      identity: "Explorer",
      versionSearch: "MSIE"
    },
    {
      string: navigator.userAgent,
      subString: "Gecko",
      identity: "Mozilla",
      versionSearch: "rv"
    },
    {     // for older Netscapes (4-)
      string: navigator.userAgent,
      subString: "Mozilla",
      identity: "Netscape",
      versionSearch: "Mozilla"
    }
  ],
  dataOS : [
    {
      string: navigator.platform,
      subString: "Win",
      identity: "Windows"
    },
    {
      string: navigator.platform,
      subString: "Mac",
      identity: "Mac"
    },
    {
         string: navigator.userAgent,
         subString: "iPhone",
         identity: "iPhone/iPod"
      },
    {
      string: navigator.platform,
      subString: "Linux",
      identity: "Linux"
    }
  ]

};
BrowserDetect.init();

/*
showSlide(id)
Displays each slide
*/

function showSlide(id) {
  $(".slide").hide();
  $("#"+id).show();
}

/*
random(a,b)
Returns random number between a and b, inclusive
*/

function random(a,b) {
  if (typeof b == "undefined") {
    a = a || 2;
    return Math.floor(Math.random()*a);
  } else {
    return Math.floor(Math.random()*(b-a+1)) + a;
  }
}


/*
Array.prototype.random
Randomly shuffles elements in an array. Useful for condition randomization.
*/

Array.prototype.random = function() {
  return this[random(this.length)];
};

/*
Produces an array with numbers 0~arrLength
in random order. Kind of spurious--use
Array.prototype.random instead
*/

function shuffledArray(arrLength)
{
  var j, tmp;
  var arr = new Array(arrLength);
  for (i = 0; i < arrLength; i++)
  {
    arr[i] = i;
  }
  for (i = 0; i < arrLength-1; i++)
  {
    j = Math.floor((Math.random() * (arrLength - 1 - i)) + 0.99) + i;
    tmp = arr[i];
    arr[i] = arr[j];
    arr[j] = tmp;
  }
  return arr;
}

/*
Gets the value of the checked radio button
*/

function getRadioCheckedValue(formNum, radio_name)
{
   var oRadio = document.forms[formNum].elements[radio_name];
   for(var i = 0; i < oRadio.length; i++)
   {
      if(oRadio[i].checked)
      {
         return oRadio[i].value;
      }
   }
   return '';
}

function setQuestion(array) {
    var i = random(0, array.length - 1);
    var q = array[i];
    return q;
}


/* 
Clears value from form
*/

function clearForm(oForm) {
    
  var elements = oForm.elements;
    
  oForm.reset();

  for(i=0; i<elements.length; i++) {
      
	field_type = elements[i].type.toLowerCase();
	
	switch(field_type) {
	
		case "text":
		case "password":
		case "textarea":
          case "hidden":

      elements[i].value = "";
			break;
        
		case "radio":
		case "checkbox":
      if (elements[i].checked) {
            elements[i].checked = false;
			}
			break;

		case "select-one":
		case "select-multi":
              elements[i].selectedIndex = -1;
			break;

		default:
			break;
	}
    }
}





// Input Data for the wheel



var allConditions = [
[
{"movieID":1, "reptition":1, "Participant":1, "Agent":1, "Ball":1, "AgentLeaves":16.7, "movieName":"OCC_P+A+" },
{"movieID":2, "reptition":1, "Participant":1, "Agent":1, "Ball":0, "AgentLeaves":16.7, "movieName":"OCC_P+A+A"},
{"movieID":3, "reptition":1, "Participant":1, "Agent":0, "Ball":1, "AgentLeaves":13.2, "movieName":"OCC_P+A-" },
{"movieID":4, "reptition":1, "Participant":1, "Agent":0, "Ball":0, "AgentLeaves":13.2, "movieName":"OCC_P+A-A"},
{"movieID":5, "reptition":1, "Participant":0, "Agent":1, "Ball":1, "AgentLeaves":10.8, "movieName":"OCC_P-A+" },
{"movieID":6, "reptition":1, "Participant":0, "Agent":1, "Ball":0, "AgentLeaves":10.8, "movieName":"OCC_P-A+A"},
{"movieID":7, "reptition":1, "Participant":0, "Agent":0, "Ball":1, "AgentLeaves":16.7, "movieName":"OCC_P-A-" },
{"movieID":8, "reptition":1, "Participant":0, "Agent":0, "Ball":0, "AgentLeaves":16.7, "movieName":"OCC_P-A-A"},
{"movieID":1, "reptition":2, "Participant":1, "Agent":1, "Ball":1, "AgentLeaves":16.7, "movieName":"OCC_P+A+" },
{"movieID":2, "reptition":2, "Participant":1, "Agent":1, "Ball":0, "AgentLeaves":16.7, "movieName":"OCC_P+A+A"},
{"movieID":3, "reptition":2, "Participant":1, "Agent":0, "Ball":1, "AgentLeaves":13.2, "movieName":"OCC_P+A-" },
{"movieID":4, "reptition":2, "Participant":1, "Agent":0, "Ball":0, "AgentLeaves":13.2, "movieName":"OCC_P+A-A"},
{"movieID":5, "reptition":2, "Participant":0, "Agent":1, "Ball":1, "AgentLeaves":10.8, "movieName":"OCC_P-A+" },
{"movieID":6, "reptition":2, "Participant":0, "Agent":1, "Ball":0, "AgentLeaves":10.8, "movieName":"OCC_P-A+A"},
{"movieID":7, "reptition":2, "Participant":0, "Agent":0, "Ball":1, "AgentLeaves":16.7, "movieName":"OCC_P-A-" },
{"movieID":8, "reptition":2, "Participant":0, "Agent":0, "Ball":0, "AgentLeaves":16.7, "movieName":"OCC_P-A-A"},
{"movieID":1, "reptition":3, "Participant":1, "Agent":1, "Ball":1, "AgentLeaves":16.7, "movieName":"OCC_P+A+" },
{"movieID":2, "reptition":3, "Participant":1, "Agent":1, "Ball":0, "AgentLeaves":16.7, "movieName":"OCC_P+A+A"},
{"movieID":3, "reptition":3, "Participant":1, "Agent":0, "Ball":1, "AgentLeaves":13.2, "movieName":"OCC_P+A-" },
{"movieID":4, "reptition":3, "Participant":1, "Agent":0, "Ball":0, "AgentLeaves":13.2, "movieName":"OCC_P+A-A"},
{"movieID":5, "reptition":3, "Participant":0, "Agent":1, "Ball":1, "AgentLeaves":10.8, "movieName":"OCC_P-A+" },
{"movieID":6, "reptition":3, "Participant":0, "Agent":1, "Ball":0, "AgentLeaves":10.8, "movieName":"OCC_P-A+A"},
{"movieID":7, "reptition":3, "Participant":0, "Agent":0, "Ball":1, "AgentLeaves":16.7, "movieName":"OCC_P-A-" },
{"movieID":8, "reptition":3, "Participant":0, "Agent":0, "Ball":0, "AgentLeaves":16.7, "movieName":"OCC_P-A-A"},
{"movieID":1, "reptition":4, "Participant":1, "Agent":1, "Ball":1, "AgentLeaves":16.7, "movieName":"OCC_P+A+" },
{"movieID":2, "reptition":4, "Participant":1, "Agent":1, "Ball":0, "AgentLeaves":16.7, "movieName":"OCC_P+A+A"},
{"movieID":3, "reptition":4, "Participant":1, "Agent":0, "Ball":1, "AgentLeaves":13.2, "movieName":"OCC_P+A-" },
{"movieID":4, "reptition":4, "Participant":1, "Agent":0, "Ball":0, "AgentLeaves":13.2, "movieName":"OCC_P+A-A"},
{"movieID":5, "reptition":4, "Participant":0, "Agent":1, "Ball":1, "AgentLeaves":10.8, "movieName":"OCC_P-A+" },
{"movieID":6, "reptition":4, "Participant":0, "Agent":1, "Ball":0, "AgentLeaves":10.8, "movieName":"OCC_P-A+A"},
{"movieID":7, "reptition":4, "Participant":0, "Agent":0, "Ball":1, "AgentLeaves":16.7, "movieName":"OCC_P-A-" },
{"movieID":8, "reptition":4, "Participant":0, "Agent":0, "Ball":0, "AgentLeaves":16.7, "movieName":"OCC_P-A-A"},
{"movieID":1, "reptition":5, "Participant":1, "Agent":1, "Ball":1, "AgentLeaves":16.7, "movieName":"OCC_P+A+" },
{"movieID":2, "reptition":5, "Participant":1, "Agent":1, "Ball":0, "AgentLeaves":16.7, "movieName":"OCC_P+A+A"},
{"movieID":3, "reptition":5, "Participant":1, "Agent":0, "Ball":1, "AgentLeaves":13.2, "movieName":"OCC_P+A-" },
{"movieID":4, "reptition":5, "Participant":1, "Agent":0, "Ball":0, "AgentLeaves":13.2, "movieName":"OCC_P+A-A"},
{"movieID":5, "reptition":5, "Participant":0, "Agent":1, "Ball":1, "AgentLeaves":10.8, "movieName":"OCC_P-A+" },
{"movieID":6, "reptition":5, "Participant":0, "Agent":1, "Ball":0, "AgentLeaves":10.8, "movieName":"OCC_P-A+A"},
{"movieID":7, "reptition":5, "Participant":0, "Agent":0, "Ball":1, "AgentLeaves":16.7, "movieName":"OCC_P-A-" },
{"movieID":8, "reptition":5, "Participant":0, "Agent":0, "Ball":0, "AgentLeaves":16.7, "movieName":"OCC_P-A-A"}
],
[
{"condition":2}
]
];






/* Experimental Variables */
// Number of conditions in experiment
var numConditions = 1; //allConditions.length;

// Randomly select a condition number for this particular participant
var chooseCondition = 1; // random(0, numConditions-1);

// Based on condition number, choose set of input (trials)
var allTrialOrders = allConditions[chooseCondition-1];

// Number of trials in each condition
var numTrials = 40; //not necessarily allTrialOrders.length;

// Produce random order in which the trials will occur
var shuffledOrder = shuffledArray(allTrialOrders.length);

// Keep track of current trial
var currentTrialNum = 0;

// A variable special for this experiment because we're randomly
// choosing word orders as well
// var wordOrder = 100;
var trial;

// Keep track of how many trials have been completed
var numComplete = 0;



/*
Show the instructions slide — this is what we want subjects to see first.
*/

if (BrowserDetect.browser != 'Chrome' && BrowserDetect.browser != 'Safari' && BrowserDetect.browser != 'Firefox') {
    alert ("Warning: We have not tested this HIT with your browser. We recommend Chrome, Firefox or Safari");
    $("#startButton").attr("disabled", "disabled");
}

$("#progressBar").hide();
showSlide("instructions");


// Updates the progress bar
$("#trial-num").html(numComplete);
$("#total-num").html(numTrials);

/*
The actual variable that will be returned to MTurk. The experiment object with various variables that you want to keep track of and return as results.

More practically, you should stick everything in an object and submit that whole object so that you don’t lose data (e.g. randomization parameters, what condition the subject is in, etc). Don’t worry about the fact that some of the object properties are functions — mmturkey (the Turk submission library) will strip these out.
*/

var experiment = {

/*
Parameters for this sequence.
*/
  condition: 1,

  // An array of subjects' responses to each trial (NOTE: in the order in which
  // you initially listed the trials, not in the order in which they appeared)
  //results: new Array(numTrials),

  // The order in which each trial appeared
  //orders: new Array(numTrials),

  // The order in which each trial is presented. i.e.
  // presentationOrder[i] = j means the i-th trial is the j-th one in the trial sequence.
  // Note that presentationOrder is now obsolete with spinnerIDArray
  // presentationOrder: new Array(numTrials),

  movieIDArray: new Array(numTrials),
  reptitionArray: new Array(numTrials),
  PArray: new Array(numTrials),
  AArray: new Array(numTrials),
  BArray: new Array(numTrials),
  // My Results:
  
  startTime: 0,
  endTime: 0,
  agentStartTime: 0,
  agentEndTime: 0,
    agentReactionTimeArray: new Array(numTrials),
    reactionTimeArray: new Array(numTrials),
    agentAccuracyArray: new Array(numTrials),
    accuracyArray: new Array(numTrials),
    

  // Demographics
  browser:"",
  comments:"",

 //trials: myTrialOrder,

/*
An array to store the data that we’re collecting.
*/

  data: [],

// Goes to description slide
  description: function() {
    $("#progressBar").show();
    showSlide("description");
    $("#tot-num").html(numTrials);

    if (turk.previewMode) {
      alert ( "Please accept the HIT before continuing." );
    }
  },

/*
The function that gets called when the sequence is finished.
*/

  end: function() {
	  // Records demographics
    var comm = document.comments.input.value;
    experiment.comments = comm;
    
    // Show the finish slide.
    showSlide("finished");

    /*
    Wait 1.5 seconds and then submit the whole experiment object to Mechanical Turk (mmturkey filters out the functions so we know we’re just submitting properties [i.e. data])
    */
    setTimeout(function() { turk.submit(experiment);}, 1500);
  },


  next: function() {
  
  showSlide("stage");
  $("#nextButton").hide();
  
  if (numComplete === 0) {
    experiment.browser=BrowserDetect.browser;

    //    mp4Frame = document.getElementById('mp4src');
    //    oggFrame = document.getElementById('oggsrc');
    //    webmFrame = document.getElementById('webmsrc');
    videoElement = document.getElementById("videoElement");
  }

  // If this is not the first trial, record variables
      if (numComplete > 0) {
            // experiment.ChooseLeft[numComplete-1] = $('input[name="q1"]:checked').val()==1;
            // experiment.Cuteness[numComplete-1] = $('input[name="q2"]:checked').val();
            // $('input[name="q1"]:').prop('checked', false);
            // $('input[name="q2"]:').prop('checked', false);
            
            experiment.movieIDArray[numComplete-1] = trial.movieID;
            experiment.reptitionArray[numComplete-1] = trial.reptition;
            experiment.PArray[numComplete-1] = trial.Participant;
            experiment.AArray[numComplete-1] = trial.Agent;
            experiment.BArray[numComplete-1] = trial.Ball;
            
            experiment.data.push(trial);
      }
      
      // If subject has completed all trials, update progress bar and
      // show slide to ask for demographic info
      if (numComplete >= numTrials) {
          $('.bar').css('width', (200.0 * numComplete/numTrials) + 'px');
          $("#trial-num").html(numComplete);
          $("#total-num").html(numTrials);
          showSlide("askInfo");
      
      } else {
        // Otherwise, if trials not completed yet, update progress bar
        // and go to next trial based on the order in which trials are supposed
        // to occur

        $('.bar').css('width', (200.0 * numComplete/numTrials) + 'px');
        $("#trial-num").html(numComplete);
        $("#total-num").html(numTrials);


        //currentTrialNum is used for randomizing later
        currentTrialNum = shuffledOrder[numComplete];
        trial = allTrialOrders[currentTrialNum];



        /*
        mp4Frame.src = "video/" + trial.movieName + ".mp4";
        oggFrame.src = "video/" + trial.movieName + ".ogv";
        webmFrame.src = "video/" + trial.movieName + ".webm";
        */

       if (videoElement.canPlayType("video/mp4")) {
            videoElement.setAttribute("src", "videos/" + trial.movieName + ".mp4");
           //window.alert("can play mp4");
       }
       else if (videoElement.canPlayType("video/webm")) {
           videoElement.setAttribute("src", "videos/" + trial.movieName + ".webm");
           //window.alert("can play webm");
       }
       else if (videoElement.canPlayType("video/ogg")) {
           videoElement.setAttribute("src", "videos/" + trial.movieName + ".ogv");
           //window.alert("can play ogg");
       }
       else {
           window.alert("Can't play anything");
       }
       videoElement.load();

        numComplete++;
        $('#agentDetected').html("");
        $('#ballDetected').html("");


        

        var reactionStart = 0;
        // reactionStart = 0 means movie has not played
        // reactionStart = 1 means movie has played, but agent has not left scene yet
        // reactionStart = 2 means movie has played, agent left scene, waiting for reactionTime.

        var keyPressHandler = function(event) {
          var keyCode = event.which;

          switch(keyCode)
          { // 32 == Spacebar, 81 = q, 80 = p, 66 = b
            case 66: // b
              if (reactionStart===0) {
                videoElement.play();
                reactionStart=1;
                setTimeout(function () {
                  experiment.agentStartTime = (new Date()).getTime();
                  $(document).one("keydown", keyPressHandler);
                },trial.AgentLeaves*1000);

                setTimeout(function() {
                  if(reactionStart==1){
                    var event1 = $.Event("keydown");
                    event1.which = "agentTimeout";
                    $(document).trigger(event1);
                  }
                  }, trial.AgentLeaves*1000 + 3000); // set agent timeout to occur 3s later

                setTimeout(function () {
                  experiment.startTime = (new Date()).getTime();
                  $(document).one("keydown", keyPressHandler);
                },21000);

                setTimeout(function() {
                  if(reactionStart==2){
                      var event1 = $.Event("keydown");
                      event1.which = "reactionTimeout";
                      $(document).trigger(event1);
                    }
                  }, 24000); // set reaction timeout to occur at 24s

              } else {
                $(document).one("keydown", keyPressHandler);
              }

            break; // end of spacebar case
            case 81: // q is pressed

              if (reactionStart==1) {
                experiment.agentEndTime = (new Date()).getTime();
                experiment.agentReactionTimeArray[numComplete-1] = experiment.agentEndTime - experiment.agentStartTime;
                experiment.agentAccuracyArray[numComplete-1] = 1;
                reactionStart=2;
                $('#agentDetected').html("Character leaving was detected!");
              } else {
                $(document).one("keydown", keyPressHandler);
              }

            break; // end of q case
            case 80: // p is pressed

              if (reactionStart==2) { // waiting for reactionTime
                reactionStart=3; // reactionStart=3 --> reaction was recorded.
                experiment.endTime = (new Date()).getTime();
                experiment.reactionTimeArray[numComplete-1] = experiment.endTime - experiment.startTime;
                if (trial.Ball==1) { // ball is present

                  experiment.accuracyArray[numComplete-1] = 1;
                  $('#ballDetected').html("Ball was detected!");

                } else { // ball is absent but p is pressed

                  experiment.accuracyArray[numComplete-1] = 0;
                  $('#ballDetected').html("Ball is not in the scene. Incorrect answer!");

                }
                videoElement.pause();
                $("#nextButton").show();
              } else {
                $(document).one("keydown", keyPressHandler);
              }

            break; // end of p case
            case "agentTimeout": // agentTimeout

                experiment.agentEndTime = (new Date()).getTime();
                experiment.agentReactionTimeArray[numComplete-1] = experiment.agentEndTime - experiment.agentStartTime;
                reactionStart=2;
                experiment.agentAccuracyArray[numComplete-1] = 0;
                $('#agentDetected').html("Too slow in detecting character leaving!");

            break; // end of agentTimeout case
            case "reactionTimeout": // reactionTimeout
                experiment.endTime = (new Date()).getTime();
                experiment.reactionTimeArray[numComplete-1] = experiment.endTime - experiment.startTime;
                
                if (trial.Ball==1) { // ball is present and timeout

                  experiment.accuracyArray[numComplete-1] = 0;
                  $('#ballDetected').html("Too slow in detecting ball!");

                } else { // ball is absent and timeout

                  experiment.accuracyArray[numComplete-1] = 1;
                  $('#ballDetected').html("Ball is not in the scene. Correct answer!");
                
                }
                videoElement.pause();
                $("#nextButton").show();
            break; // end of reactionTimeout case
            default:
            $(document).one("keydown", keyPressHandler);
          }
        };
          $(document).one("keydown", keyPressHandler);

          /*
            if (keyCode != 32 && keyCode != 81 && keyCode != 80 && keyCode != "reactionTimeout" && keyCode != "agentTimeout") {
            // 32 == Spacebar, 81 = q, 80 = p
              $(document).one("keydown", keyPressHandler);

            } else if (keyCode == 81) { // q is pressed

              if (reactionStart==1) {
                experiment.agentEndTime = (new Date()).getTime();
                experiment.agentReactionTimeArray[numComplete-1] = experiment.agentEndTime - experiment.agentStartTime;
                experiment.agentAccuracyArray[numComplete-1] = 1;
                reactionStart=2;
                $('#agentDetected').html("Character leaving was detected!");
              } else {
                $(document).one("keydown", keyPressHandler);
              }

            } else if (keyCode == 80) { // p is pressed

              if (reactionStart==2) { // waiting for reactionTime
                experiment.endTime = (new Date()).getTime();
                experiment.reactionTimeArray[numComplete-1] = experiment.endTime - experiment.startTime;
                if (trial.Ball==1) { // ball is present

                  experiment.accuracyArray[numComplete-1] = 1;
                  $('#ballDetected').html("Ball was detected!");

                } else { // ball is absent but p is pressed

                  experiment.accuracyArray[numComplete-1] = 0;
                  $('#ballDetected').html("Ball is not in the scene. Incorrect answer!");

                }
                videoElement.pause();
                $("#nextButton").show();
              } else {
                $(document).one("keydown", keyPressHandler);
              }

            } else if (keyCode == 32) { // spacebar is pressed
              
              if (reactionStart===0) {
                videoElement.play();
                reactionStart=1;
                setTimeout(function () {
                  experiment.agentStartTime = (new Date()).getTime();
                  $(document).one("keydown", keyPressHandler);
                },trial.AgentLeaves*1000);

                setTimeout(function() {
                  var event = $.Event("keydown");
                  event.which = "agentTimeout";
                  $(document).trigger(event);
                }, trial.AgentLeaves*1000 + 4000); // set agent timeout to occur 4s later

                setTimeout(function () {
                  experiment.startTime = (new Date()).getTime();
                  $(document).one("keydown", keyPressHandler);
                },21000);

                setTimeout(function() {
                  var event = $.Event("keydown");
                  event.which = "reactionTimeout";
                  $(document).trigger(event);
                }, 25000); // set reaction timeout to occur at 25s

              } else {
                $(document).one("keydown", keyPressHandler);
              }
            } else if (keyCode == "agentTimeout") {
                experiment.agentEndTime = (new Date()).getTime();
                experiment.agentReactionTimeArray[numComplete-1] = experiment.agentEndTime - experiment.agentStartTime;
                reactionStart=2;
                experiment.agentAccuracyArray[numComplete-1] = 0;
                $('#agentDetected').html("Too slow in detecting character leaving!");
            } else if (keyCode == "reactionTimeout") {
                experiment.endTime = (new Date()).getTime();
                experiment.reactionTimeArray[numComplete-1] = experiment.endTime - experiment.startTime;
                
                if (trial.Ball==1) { // ball is present and timeout

                  experiment.accuracyArray[numComplete-1] = 0;
                  $('#ballDetected').html("Too slow in detecting ball!");

                } else { // ball is absent and timeout

                  experiment.accuracyArray[numComplete-1] = 1;
                  $('#ballDetected').html("Ball is not in the scene. Correct answer!");
                
                }
                videoElement.pause();
                $("#nextButton").show();

            }
          };
          $(document).one("keydown", keyPressHandler);
      */
        

       } // end of experiment.next's else block (numComplete < numTrials)
          // experiment.data.push(data);
          //setTimeout(experiment.next, 500);
  } // end of experiment.next();
}; // end of experiment variable

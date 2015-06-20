function main(subjID)
% Modified Jorie Koster-Hale's vidloop code and PsychToolBox's
% DetectionRTinVideo demo
%e.g. vidloop('sub_01')

%% Stimulus Directories

rootdir = '/Users/jeanxin/Documents/MIT Spring 2013/9.61/Experiment/';   %where the whole experiment is saved
behavDir = fullfile(rootdir,'behavioral');   %where the data are saved  (a subfolder in the folder)
stimDir = fullfile(rootdir,'stimuli');  %where the movie names are saved  (a subfolder in the folder)
vidDir = fullfile(rootdir,'stimuli/videos');  %where the movie files are saved  (a subfolder in the folder)

%make sure the psychtool box will work
PsychJavaTrouble;

%go to the folder with the stimuli list
cd(stimDir);

%% Stimuli, Timing, and Keys

%movie_list.ma t contains the list of animations presented in the following
%format: P(participant belief)A(agent belief)B(actually there?). (e.g.
%P+A+B+)

%each matrix is organized such that each column is an order of conditions

load movie_list.mat;

%timing
restDuration = 1;  %time in seconds for between trials

%get the number of conditions and number of trials
numCond = size(movie_list);
numTrials = 5;
totalTrials=numTrials*numCond;

% Switch KbName into unified mode: It will use the names of the OS-X
% platform on all platforms in order to make this script portable:
KbName('UnifyKeyNames');

% Query keycodes for ESCAPE key and 1 and 0 keys:
esc=KbName('ESCAPE');
zerokey=KbName('0)');
onekey=KbName('1!');

%start recording RT from this point at each movie (0 means start of event)
timeOfAgent = 14.5; %check with Sammy
timeOfBall=16; %check with Sammy

%% Experiment Parameters

%randomization seed:
%assumes that ID is in the form of string_number (e.g. "subj_01")
[~, name] = strtok(subjID,'_');
[randseed, ~] = strtok(name,'_');
disp('Using randseed...');
IDnum = str2num(randseed);
rand('twister',IDnum);


%counter-balancing:  order that the conditions will be presented in
[presentSet, trialCount]=counterbalance(IDnum);

%creates matrix to match condition order to specific stories for this run
%make the list of movies, in order, that participant will see
presentMovie={};
for i = 1:length(presentSet)
    presentMovie{i,1} = movie_list{presentSet(i)};
end

%make matrices for behavioral reponses
ballRTs=zeros(1,totalTrials);
agentRTs=zeros(1,totalTrials);
movieOrder=cell2mat(presentMovie);

%% PsychTookBox Parameters

% Child protection: Make sure we run on the OSX / OpenGL Psychtoolbox.
% Abort if we don't:
AssertOpenGL;

% Background color will be a grey one:
background=[128, 128, 128];

% Open onscreen window. We use the display with the highest number on
% multi-display setups:
screen=max(Screen('Screens'));

% This will open a screen with default settings, aka black background,
% fullscreen, double buffered with 32 bits color depth:
win = Screen('OpenWindow', screen);

% Hide the mouse cursor:
HideCursor;

% Clear screen to background color:
Screen('FillRect', win, background);

%Open all the movie names
cd(vidDir)

%% Instructions and Trigger

% Show instructions...
tsize=30;
Screen('TextSize', win, tsize);
[x, y]=Screen('DrawText', win, 'Visual Detection Task',40, 100);
[x, y]=Screen('DrawText', win, 'Press ESC-ape key to abort anytime.', 40, y + 10 + tsize);
[x, y]=Screen('DrawText', win, 'Press "1" key when you see the smurf leave the scene', 40, y + 10 + tsize);
[x, y]=Screen('DrawText', win, 'Press "0" key when you see the ball appear behind the occluder', 40, y + 10 + tsize);

Screen('DrawText', win, 'Press any key to start the experiment...', 40, y + 10 + tsize);

% Flip to show the grey screen:
Screen('Flip',win);

% Wait for keypress + release...
KbStrokeWait;

% Show cleared screen...
Screen('Flip',win);

% Wait a second...
WaitSecs(1);

%% Main Experiment1
experimentStart = GetSecs;

%pause for opening fixation
while GetSecs - experimentStart < restDuration; end

%% Main trial loop: Do 'trials' trials...
for trial=1:totalTrials
    FlushEvents;
    
    %set the current movie
    movieName = ['/Users/jeanxin/Documents/MIT Spring 2013/9.61/Experiment/stimuli/videos/' presentMovie{trial} '.avi'];
    % Open the moviefile and query some infos like duration, framerate,
    % width and height of video frames. We could also query the total count of frames in
    % the movie, but computing 'framecount' takes long, so avoid to query
    % this property if you don't need it!
    [movie movieduration fps] = Screen('OpenMovie', win, movieName);
    
    % We estimate framecount instead of quer1ying it - faster:
    framecount = movieduration * fps;
    
    % Start playback of the movie:
    % Play 'movie', at a playbackrate = 1 (normal speed forward),
    % play it once, aka with loopflag = 0,1
    % play audio track at volume 1.0  = 100% audio volume.
    Screen('PlayMovie', movie, 1, 0, 1.0);
    
    %% Video Playback
    % Video playback and key response RT collection loop:
    % This loop repeats until either the subject responded with a
    % keypress to indicate s(he) detected the event in the vido, or
    % until the end of the movie is reached.
    movietexture=0;     % Texture handle for the current movie frame.
    lastpts=0;          % Presentation timestamp of last frame.
    reactiontime1=-1;    % Variable to store reaction time for ball.
    onsettime1=-1;       % Realtime at which the ball was shown to the subject.
    rejecttrial1=0;      % Flag which is set to 1 to reject an invalid trial for ball.
    reactiontime2=-1;    % Variable to store reaction time for agent.
    onsettime2=-1;       % Realtime at which the agent was shown to the subject.
    rejecttrial2=0;      % Flag which is set to 1 to reject an invalid trial for agent.
    
    while(movietexture>=0 && reactiontime1==-1)
        [movietexture pts] = Screen('GetMovieImage', win, movie, 0);
        
        % Is it a valid texture?
        if (movietexture>0)
            % Yes. Draw the texture into backbuffer:
            Screen('DrawTexture', win, movietexture);
            
            % Flip the display to show the image at next retrace:
            % vbl will contain the exact system time of image onset on
            % screen: This should be accurate in the sub-millisecond
            % range.
            
            vbl=Screen('Flip', win);
            
            % AGENT
            % Is this the event video frame we've been waiting for?
            if (onsettime2==-1 && pts >= timeOfAgent)
                % Yes: This is the first frame with a pts timestamp that is
                % equal or greater than the timeOfEvent, so 'vbl' is
                % the exact time when the event was presented to the
                % subject. Define it as onsettime:
                onsettime2 = vbl;
                
                % Compare current pts to last one to see if the movie
                % decoder skipped a frame at this crucial point in
                % time. That would invalidate this trial.
                if (pts - lastpts > 1.5*(1/fps))
                    % Difference to last frame is more than 1.5 times
                    % the expected difference under assumption 'no
                    % skip'. We skipped in the wrong moment!
                    rejecttrial2=1;
                end;
            end;
            
            %BALL
            if (onsettime1==-1 && pts >= timeOfBall)
                onsettime1 = vbl;
                if (pts - lastpts > 1.5*(1/fps))
                    rejecttrial1=1;
                end;
            end;
            
            % Keep track of the frames pts in order to check for skipped frames:
            lastpts=pts;
            
            % Delete the texture. We don't need it anymore:
            Screen('Close', movietexture);
            movietexture=0;
        end;
        
        % Done with drawing. Check the keyboard for subjects response:
        [keyIsDown, secs, keyCode]=KbCheck;
        if (keyIsDown==1)
            % Abort requested?
            if keyCode(esc)
                % This signals abortion:
                rejecttrial1=-1;
                % Break out of display loop:
                break;
            end;
            
            % One key pressed to indicate detection of agent?
            if keyCode(onekey)
                % Response too early (before event happened?)
                if (onsettime2==-1)
                    % Reject this trial:
                    rejecttrial2=2;
                else
                    % Valid response: Difference between 'secs' and
                    % 'onsettime'1 the reaction time:
                    reactiontime2=secs - onsettime2;
                    agentRTs(trial)=reactiontime2;
                end;
            end
            
            % Zero key pressed to indicate detection of ball?
            if keyCode(zerokey)
                % Response too early (before event happened?)
                if (onsettime1==-1)
                    % Reject this trial:
                    rejecttrial1=2;
                else
                    % Valid response: Difference between 'secs' and
                    % 'onsettime' is the reaction time:
                    reactiontime1=secs - onsettime1;
                    ballRTs(trial)=reactiontime1;
                end;
            end;
        end;
    end; % ...of display loop...
    
    % Stop movie playback, in case it isn't already stopped. We do this
    % by selection of a playback rate of zero: This will also return
    % the number of frames that had to be dropped to keep audio, video
    % and realtime in sync.
    droppedcount = Screen('PlayMovie', movie, 0, 0, 0);
    if (droppedcount > 0.2*framecount)
        % Over 20% of all frames skipped?!? Playback problems! We
        % reject this trial...
        rejecttrial1=4;
    end;
    
    % Close the moviefile.
    Screen('CloseMovie', movie);
    Screen('Flip',win);
    Screen('Close');
    
    %pause for the fixation between trials
    endTime = GetSecs;
    while GetSecs - endTime < restDuration;
    end
    
    %% Results
    % Check if aborted.
    if (rejecttrial1==-1)
        % Break out of trial loop
        break;
    end;
    
    if (reactiontime1==-1 && rejecttrial1==0)
        rejecttrial1=3;
    end;
    
    % Print out trials result if it was a valid trial:
    if (rejecttrial1==0)
        fprintf('Trial %i valid: Reaction time was %f msecs.\n', trial, 1000 * reactiontime1);
    end;
    
    if (rejecttrial1==1)
        fprintf('Trial %i rejected due to skip in video playback at time of event.\n', tria1);
    end;
    
    if (rejecttrial1==2)
        fprintf('Trial %i rejected. False detection by subject.\n', trial);
    end;
    
    if (rejecttrial1==3)
        fprintf('Trial %i rejected. No ball appears. \n', trial);
    end;
    
    if (rejecttrial1==4)
        fprintf('Trial %trial rejected. Way too many skips in movie playback!!!\n', trial);
    end;
    
    % Wait for subject to release keys:
    KbReleaseWait;
    
end; % Trial done. Next trial...
Screen('Flip',win);
experimentEnd = GetSecs;

%save the data (plus information about the trials)
ball=zeros(1,40);
for i=1:length(presentSet)
    p=presentSet(i,1);
    if p==1 || p==2 || p==3 ||p==4
        ball(i)=1;
    end
end

falseBelief=zeros(1,40);
for i=1:length(presentSet)
    p=presentSet(i,1);
    if p==3||p==4||p==7||p==8
        falseBelief(i)=1;
    end
end
cd(behavDir);
save([subjID '.mat'],'agentRTs','ballRTs', 'movieOrder', 'presentSet', 'trialCount', 'ball', 'falseBelief');
s=struct('agentRTs',agentRTs,'ballRTs', ballRTs,'presentSet', presentSet, 'trialCount', trialCount, 'ball', ball, 'falseBelief', falseBelief);
dwrite(s, [subjID '.csv'])
Screen('CloseAll');
cd(rootdir)
clear all;
%main function

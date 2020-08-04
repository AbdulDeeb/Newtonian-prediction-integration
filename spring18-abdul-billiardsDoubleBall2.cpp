// This file is part of CNCSVision, a computer vision related library
// This software is developed under the grant of Italian Institute of Technology
//
// Copyright (C) 2011 Carlo Nicolini <carlo.nicolini@iit.it>
//
// CNCSVision is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 3 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// CNCSVision is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License or the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License and a copy of the GNU General Public License along with
// CNCSVision. If not, see <http://www.gnu.org/licenses/>.

#include <cstdlib>
#include <cmath>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <vector>
#include <string>
#include <map>
#include <Eigen/Core>
#include <Eigen/Geometry>
#include <algorithm>
#include <queue>

/********* BOOST MULTITHREADED LIBRARY ****************/
#include <boost/thread/thread.hpp>
#include <boost/asio.hpp>	//include asio in order to avoid the "winsock already declared problem"

#ifdef __APPLE__
#include <OpenGL/OpenGL.h>
#include <GLUT/glut.h>
#endif

#ifdef __linux__
#include <GL/glut.h>
#include <SOIL/SOIL.h>
#endif

#ifdef _WIN32
#include <windows.h>
#include <gl\gl.h>            // Header File For The OpenGL32 Library
#include <gl\glu.h>            // Header File For The GLu32 Library
#include "glut.h"            // Header File For The GLu32 Library
#include <MMSystem.h>
#include "SOIL.h"
#endif
/********* INCLUDE CNCSVISION LIBRARY HEADERS **********/
//#include "Optotrak.h"
#include "Optotrak2.h"
#include "Marker.h"
#include "Mathcommon.h"
#include "GLUtils.h"
#include "VRCamera.h"
#include "CoordinatesExtractor.h"
#include "CylinderPointsStimulus.h"
#include "EllipsoidPointsStimulus.h"
#include "StimulusDrawer.h"
#include "GLText.h"
#include "BalanceFactor.h"
#include "ParStaircase.h"
#include "Staircase.h"
#include "ParametersLoader.h"
#include "TrialGenerator.h"
#include "Util.h"
#include "BrownMotorFunctions.h"
#include "BrownPhidgets.h"

/***** CALIBRATION FILE *****/
#include "LatestCalibration.h"

/***** DEFINE SIMULATION *****/
//#define SIMULATION
#ifndef SIMULATION
	#include <direct.h> // mkdir
#endif

/********* NAMESPACE DIRECTIVES ************************/
using namespace std;
using namespace mathcommon;
using namespace Eigen;
using namespace util;
using namespace BrownMotorFunctions;
using namespace BrownPhidgets;

/********* #DEFINE DIRECTIVES **************************/
#define TIMER_MS 11                               // 85 hz
#define SCREEN_WIDTH  1024                  // 1024 pixels
#define SCREEN_HEIGHT 768                   // 768 pixels

double interoculardistance = 0;
const float DEG2RAD = 3.14159/180;
Screen screen;
double mirrorAlignment = 0.0;
double screenAlignmentY = 0.0;
double screenAlignmentZ = 0.0;

/********* VARIABLES OBJECTS  **************************/
VRCamera cam;
Optotrak2 optotrak;
CoordinatesExtractor headEyeCoords, thumbCoords, indexCoords, thumbJointCoords, indexJointCoords;
Timer timer;
Timer globalTimer;
clock_t t;

/********* VISUALIZATION AND STIMULI *******************/
static const bool gameMode=true;
static const bool stereo=true;

/********* MARKERS AND 3D VECTORS ****************************/
// fingers markers numbers
int ind0 = 3;
int ind1 = 13, ind2 = 14, ind3 = 16;
int thu1 = 15, thu2 = 17, thu3 = 18;
int calibration1 = 1, calibration2 = 2;
int screen1 = 19, screen2 = 20, screen3 = 21;
int mirror1 = 6, mirror2 = 22;
int centercalMarker = 4;

Vector3d eyeLeft, eyeRight;
Vector3d ind, thm;
Vector3d indexCalibrationPoint(0,0,0), thumbCalibrationPoint(0,0,0);

vector <Marker> markers;

/********* VISIBILITY BOOLEANS ************************/
bool markers_status = true;
bool allVisibleIndex=markers_status;
bool allVisibleThumb=markers_status;
bool allVisibleFingers=markers_status;

bool visibleInfo=true;

/********* FILE STREAMS *************************************/
ofstream trialFile;

/*************************************************************************************/
/*** Everything above this point stays more or less the same between experiments.  ***/
/*************************************************************************************/

/********* VARIABLES THAT CHANGE IN EACH EXPERIMENT *************************/
// Experiment variables
ParametersLoader parameters; //high level variables from parameters file
TrialGenerator<double> trial;

// FOR NEW CALIBRATION:
double markerXOffset = 10;
double markerYOffset = 10;

// Variables for counting trials, frames, and lost frames
int trialNumber = 0;
int frameN = 0;
int trialsPerBlock = 36;

// Flags for important states in the experiment
bool fingersCalibrated = false;
bool contactOccurred = false;
bool cueBallStruck = false;
bool cueVelSet = false;
int activePhase;
bool finished = false;

// Display
double displayDepth = -400;
double distanceCue2TargetZ= 100;
// finger pos
float indXNow,indYNow,indZNow;

// Virtual target objects
float targetCenter_x = 0;
float targetCenter_y = 0;
float targetCenter_z = 0;
float targetRadius = 16; 
float targetVel_x = 0;
float targetVel_y = 0;
float targetVel_z = 0;
float targetVel_x_temp = 0;
float targetVel_z_temp = 0;
float compCenter_x = 0;
float compCenter_z = 0;
float compVel_x = 0;
float compVel_z= 0;
float compVel_x_temp = 0;
float compVel_z_temp = 0;

float cueCenter_x = 0;
float cueCenter_y = 0;
float cueCenter_z = 0;
float cueRadius = 16;
float cueVel_x = 0;
float cueVel_y = 0;
float cueVel_z = 0;
float cueVel_x_temp = 0;
float cueVel_z_temp = 0;
float cPointX,cPointY,cPointZ;
float compAngle;

float movementX = 0;
float movementY = 0;
float movementZ = 0;
double lineVelMagnitude;
double responseDelay = 600; 
double timeOfContact;
float distanceBetween; 
float pointRotation;

double elapsed;

int occlusion;
int cueBallPosition;
int compPosition;
double aimingAngle;
double deflectionAngle;
double cueZX_ratio;
double targetZX_ratio;

double vecLength_unnorm;
double CompVecLength_unnorm;
double speed = 3.5;
double speed2 = 4.5;

double distanceToCueBall = 999;

bool handAtStart;
float distanceToStart;

float beta = DEG2RAD*15; // this determines where the target points are
float gamma = DEG2RAD*8; // this determines the deflectionAngle of bad physics
float point_x;
float point_z;
float delta_x2;
float delta_z2;
float alpha2;
float point_x_cue;
float point_z_cue;
float point_x_cue2;
float point_z_cue2;

bool response;


// Incremented when stepping thru calibration procedure
// Make sure that drawInfo() and handleKeypress() are in agreement about this variable!
int fingerCalibrationDone = 0;

GLfloat LightAmbient[] = {0.0f, 0.0f, 0.0f, 1.0f}; 
GLfloat LightDiffuse[] = {1.0f, 1.0f, 1.0f, 1.0f};
GLfloat LightSpecular[] = {1.0f, 1.0f, 1.0f, 1.0f};
GLfloat LightPosition[] = {20.0f, 150.0f, 0.0f, 1.0f};
GLfloat specularMaterial[] = {1.0, 0.0, 0.0, 1.0};
GLfloat shininessMaterial = 32.0f;
// To use the motors:
// 0. homeEverything(arm speed, screen speed) brings all motors to their start positions, nearest to the mirror.
// 1. Before trying to move, you must pick a point that is rigidly attached to the workspace robotic arm.
// 2. Call this location 'centercal' and simply hold onto this initial location of your 'point of interest'.
// 3. When moving the arm you will decide where to put this point in particular.
// 4. Provide the desired final location and the initial location of the point of interest: moveObjectAbsolute(moveTo, centercal)
// Repeat: Set centercal to be the starting position (after homeEverything) of the point you want to control, then select locations for this point
Vector3d centercal(0.0, 0.0, 0.0);
//double centercalToRotationAxisAtStart = 23; // centercal marker has a y-offset from the desired point of interest
// Settable Target Locations
Vector3d moveTo(0.0,-5000,-500);

/********** FUNCTION PROTOTYPES *****/
void advanceTrial();
void beepOk(int tone);
void calibration_fingers(int phase);
void cleanup();
void drawGLScene();
void drawInfo();
void drawFingers();
void drawStimulus();
void drawResponseGrid();
void handleKeypress(unsigned char k, int x, int y);
void handleResize(int w, int h);
void idle();
void initMotors();
void initOptotrak();
void initProjectionScreen(double _focalDist, const Affine3d &_transformation=Affine3d::Identity(),bool synchronous=true);
void initRendering();
void initStreams();
void initTrial();
void initVariables();
void update(int value);
void updateTheMarkers();

// online operations
void online_apparatus_alignment();
void online_fingers();
void online_trial();

/*************************** EXPERIMENT SPECS ****************************/
// experiment directory
string experiment_directory = "R:/CLPS_Domini_Lab/abdul/spring18-billiardsDoubleBall2/";
string experiment_directory_desktop = "C:/Users/visionlab/Desktop/abdul/billiardsDoubleBall2/";
// parameters file directory and name
string parametersFile_directory = experiment_directory + "spring18-billiardsDoubleBall2Parameters.txt";
// trial file headers
string trialFile_headers = "subjName\ttrialN\tactivePhase\tocclusion\taimingAngle\tcompPosition\tspeed\tdeflectionAngle\tresponse\ttimeOfContact\telapsed\tframeN\tcueBallStruck\tcueVelSet\tcontactOccurred\tcueCenter_x\tcueCenter_y\tcueCenter_z\ttargetCenter_x\ttargetCenter_y\ttargetCenter_z\tindXraw\tindYraw\tindZraw\tcompAngle";
/*************************** FUNCTIONS ***********************************/
// First, make sure the filenames in here are correct and that the folders exist.
// If you mess this up, data may not be recorded!
void initStreams()
{
	ifstream parametersFile;
	parametersFile.open(parametersFile_directory.c_str());
	parameters.loadParameterFile(parametersFile);

	string subjectName = parameters.find("SubjectName");
	activePhase = str2num<int>(parameters.find("ActivePhase"));
	
	// trialFile directory
	string dirName  = experiment_directory_desktop + subjectName;
	mkdir(dirName.c_str()); // windows syntax

	if (util::fileExists(dirName+"/"+subjectName + "_trial1.txt"))
	{
		string error_on_file_io = dirName+"/"+subjectName+"trial1.txt" + string(" already exists");
		cerr << error_on_file_io << endl;
		MessageBox(NULL, (LPCSTR)"FILE ALREADY EXISTS\n Please check the parameters file.",NULL, NULL);
		exit(0);
	}

	globalTimer.start();
}


// Edit case 'f' to establish calibration procedure
// Also contains other helpful button presses (ESC for quit, i for info)
void handleKeypress(unsigned char key, int x, int y){
	switch (key){
	
		case 'o':
		case 'O':
		{
			visibleInfo=!visibleInfo;
		}
		break;

		case 'm':
		case 'M':
		{
			interoculardistance += 0.5;
			headEyeCoords.setInterOcularDistance(interoculardistance);
		}
		break;
		
		case 'n':
		case 'N':
		{
			interoculardistance -= 0.5;
			headEyeCoords.setInterOcularDistance(interoculardistance);
		}
		break;

		case 27:	// ESC
		{   
			homeEverything(5000,4500);
			cleanup();
			exit(0);
		}
		break;

		case 'f':
		case 'F':
		{
			if ( fingerCalibrationDone==0 && allVisibleFingers && isVisible(markers.at(ind0).p) )
			{
				// calibration on the X
				indexCalibrationPoint=markers.at(ind0).p;
				indexCalibrationPoint[0] = indexCalibrationPoint[0] - 25;
				indexCoords.init(indexCalibrationPoint, markers.at(ind1).p, markers.at(ind2).p, markers.at(ind3).p );

				fingerCalibrationDone=3;
				fingersCalibrated=true;
				visibleInfo=false;
				beepOk(0);
				trialNumber++;
				initTrial();
				break;
			}
		}
		break;

		case '1':
			{
			if (elapsed>=(timeOfContact+occlusion))  
			{		if (compPosition == 40){
					response = true;
					advanceTrial(); 
			}
			else{
				response = false;
				advanceTrial(); 
			}

			}
		}
		break;

		case '2':
		{
			if (elapsed>=(timeOfContact+occlusion))  
			{		if (compPosition == 40){
					response = false;
					advanceTrial(); 
			}
			else{
				response = true;
				advanceTrial(); 
			}

			}
		}
		
		break;
	
		
	}
}

/*** GRASP ***/
void calibration_fingers(int phase)
{
	/*
	switch (phase)
	{
		case 1:
		{
			if(isVisible(markers[calibration1].p) && isVisible(markers[calibration2].p))
			{
				indexCalibrationPoint=markers.at(calibration1).p;
				indexCalibrationPoint[0] = indexCalibrationPoint[0] - markerXOffset;
				indexCalibrationPoint[1] = indexCalibrationPoint[1] + markerYOffset;
				thumbCalibrationPoint=markers.at(calibration2).p;
				thumbCalibrationPoint[0] = thumbCalibrationPoint[0] - markerXOffset;
				thumbCalibrationPoint[1] = thumbCalibrationPoint[1] - markerYOffset;
			}
		} break;
		case 2:
		{
			indexCoords.init(indexCalibrationPoint, markers.at(ind1).p, markers.at(ind2).p, markers.at(ind3).p );
			thumbCoords.init(thumbCalibrationPoint, markers.at(thu1).p, markers.at(thu2).p, markers.at(thu3).p );
		} break; 
		case 3:
		{
			indexJointCoords.init(indexCalibrationPoint, markers.at(ind1).p, markers.at(ind2).p, markers.at(ind3).p );
			thumbJointCoords.init(thumbCalibrationPoint, markers.at(thu1).p, markers.at(thu2).p, markers.at(thu3).p );
		} break;
		
	}
	*/
}


// Provide text instructions for calibration, as well as information about status of experiment
void drawInfo()
{
	if (finished)
		visibleInfo = true;

	if ( visibleInfo )
	{
		glDisable(GL_COLOR_MATERIAL);
		glDisable(GL_BLEND);
		glDisable(GL_LIGHTING);
		GLText text;
		if ( gameMode )
			text.init(SCREEN_WIDTH,SCREEN_HEIGHT,glWhite,GLUT_BITMAP_HELVETICA_12);
		else
			text.init(640,480,glWhite,GLUT_BITMAP_HELVETICA_12);
		text.enterTextInputMode();

		if (finished) {
			glColor3fv(glWhite);
			text.draw("The experiment is over. Thank you! :)");
		}else{
			if(!fingersCalibrated){
				switch (fingerCalibrationDone)
				{
					case 0:
						text.draw("Press F when index finger is on the X.");
						break;
				} // end switch(fingerCalibrationDone)
			}

            /////// Header ////////
			text.draw("####### ####### #######");
			text.draw("#");
			text.draw("# Name: " +parameters.find("SubjectName"));
			text.draw("# IOD: " +stringify<double>(interoculardistance));
			text.draw("# Finger to Cue Distance: " +stringify<double>(distanceToCueBall));
			text.draw("# Active Phase? " +stringify<int>(activePhase));
			text.draw("# Cue-to-TargetPoint X:" +stringify<double>(delta_x2));
			text.draw("# Cue-to-TargetPoint Z:" +stringify<double>(delta_z2));
			text.draw("# Alpha 2:" +stringify<double>(alpha2));
			text.draw("# Cue Point X offset:" +stringify<double>(point_x_cue));
			text.draw("# Cue Point Z offset:" +stringify<double>(point_z_cue));
			text.draw("# Cue Ball Struck? " +stringify<int>(cueBallStruck));
			text.draw("# Contact Occurred? " +stringify<double>(contactOccurred));
			text.draw("# Hand At Start? " +stringify<double>(handAtStart));
			text.draw("# Occlusion? " +stringify<int>(occlusion));
			text.draw("# deflectionAngle? " +stringify<int>(deflectionAngle));
			text.draw("# compAngle? " +stringify<int>(compAngle));
			text.draw("# compPosition " +stringify<int>(compPosition));
			
            /////// Mirror and Screen Alignment ////////
			if ( abs(mirrorAlignment - 45.0) < 0.2 )
				glColor3fv(glGreen);
			else
				glColor3fv(glRed);
			text.draw("# Mirror Alignment = " +stringify<double>(mirrorAlignment));
			
			if ( isVisible(markers[mirror1].p) )
				glColor3fv(glGreen);
			else
				glColor3fv(glRed);
			text.draw("Mirror 1 " +stringify< Eigen::Matrix<double,1,3> > (markers[mirror1].p.transpose()));
			
			if ( isVisible(markers[mirror2].p) )
				glColor3fv(glGreen);
			else
				glColor3fv(glRed);
			text.draw("Mirror 2 " +stringify< Eigen::Matrix<double,1,3> > (markers[mirror2].p.transpose()));
			
			if ( screenAlignmentY < 89.0 )
				glColor3fv(glRed);
			else
				glColor3fv(glGreen);
			text.draw("# Screen Alignment Y = " +stringify<double>(screenAlignmentY));
			if ( abs(screenAlignmentZ) < 89.0 )
				glColor3fv(glRed);
			else
				glColor3fv(glGreen);
			text.draw("# Screen Alignment Z = " +stringify<double>(screenAlignmentZ));


            glColor3fv(glWhite);
			if (fingerCalibrationDone==0){

				if ( isVisible(markers[ind0].p) )
					glColor3fv(glGreen);
				else
					glColor3fv(glRed);
				text.draw("Index Calibration Point " +stringify< Eigen::Matrix<double,1,3> > (markers[ind0].p.transpose()));

				///// INDEX FINGER ///////
				glColor3fv(glWhite);
				text.draw(" " );
				text.draw("Index" );
				if ( isVisible(markers[13].p) && isVisible(markers[14].p) && isVisible(markers[16].p) )
					glColor3fv(glGreen);
				else
					glColor3fv(glRed);
				text.draw("Marker " + stringify<int>(13)+stringify< Eigen::Matrix<double,1,3> > (markers[13].p.transpose())+ " [mm]" );
				text.draw("Marker " + stringify<int>(14)+stringify< Eigen::Matrix<double,1,3> > (markers[14].p.transpose())+ " [mm]" );
				text.draw("Marker " + stringify<int>(16)+stringify< Eigen::Matrix<double,1,3> > (markers[16].p.transpose())+ " [mm]" );

				/////// THUMB //////
				glColor3fv(glWhite);
				text.draw(" " );
				text.draw("Thumb" );
				if ( isVisible(markers[15].p) && isVisible(markers[17].p) && isVisible(markers[18].p) )
					glColor3fv(glGreen);
				else
					glColor3fv(glRed);
				text.draw("Marker " + stringify<int>(15)+stringify< Eigen::Matrix<double,1,3> > (markers[15].p.transpose())+ " [mm]" );
				text.draw("Marker " + stringify<int>(17)+stringify< Eigen::Matrix<double,1,3> > (markers[17].p.transpose())+ " [mm]" );
				text.draw("Marker " + stringify<int>(18)+stringify< Eigen::Matrix<double,1,3> > (markers[18].p.transpose())+ " [mm]" );
			}
            
            /////// Index and Thumb Positions ////////
            if (fingersCalibrated){
				glColor3fv(glWhite);
				text.draw("--------------------");
				if (allVisibleIndex)
					glColor3fv(glGreen);
				else
					glColor3fv(glRed);
				text.draw("Index= " +stringify< Eigen::Matrix<double,1,3> >(ind.transpose()));
				/*if (allVisibleThumb)
					glColor3fv(glGreen);
				else
					glColor3fv(glRed);
				text.draw("Thumb= " +stringify< Eigen::Matrix<double,1,3> >(thm.transpose()));*/
				glColor3fv(glWhite);
				text.draw("--------------------");
            }

			//////// OTHER INFO /////
			glColor3fv(glGreen);
			text.draw("Timer= " + stringify<int>(timer.getElapsedTimeInMilliSec()) );
			text.draw("Frame= " + stringify<int>(frameN));
			glColor3fv(glWhite);
			text.draw("--------------------");
		}
		text.leaveTextInputMode();
		glEnable(GL_COLOR_MATERIAL);
		glEnable(GL_BLEND);
	}
}

// This will be called at 85hz in the main loop
// Not too much to change here usually, sub-functions do the work.
void drawGLScene() 
{
	online_apparatus_alignment();
	online_fingers();
	online_trial();

	if (stereo)
    {   glDrawBuffer(GL_BACK);
		// Draw left eye view
        glDrawBuffer(GL_BACK_LEFT);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glClearColor(0.0,0.0,0.0,1.0);
        cam.setEye(eyeLeft);
        drawStimulus();
		drawInfo();

        // Draw right eye view
        glDrawBuffer(GL_BACK_RIGHT);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glClearColor(0.0,0.0,0.0,0.0);
        cam.setEye(eyeRight);
        drawStimulus();
		drawInfo();

        glutSwapBuffers();
    }
    else
    {   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glClearColor(0.0,0.0,0.0,1.0);
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        cam.setEye(eyeRight);
        drawStimulus();
		drawInfo();
        glutSwapBuffers();
    }
}

// Can check for various conditions that might affect how the graphics in here
void drawStimulus()
{
	if(((occlusion == 60 || occlusion == 80) && elapsed<(timeOfContact+occlusion)) || // 1. in partial occlusion, during display period
		(!contactOccurred)) 
	{	
	
		
		
		glEnable(GL_LIGHTING);
		glEnable (GL_POLYGON_SMOOTH);
		glEnable (GL_BLEND);
			
		// 1. Draw target ball
		glColor3f(0.8,0.0,0.0);
		glPushMatrix();
		glLoadIdentity();
		GLUquadricObj* targetBall = gluNewQuadric();
		gluQuadricDrawStyle(targetBall, GLU_FILL);
		glMaterialfv(GL_FRONT, GL_SPECULAR, specularMaterial);
		glMaterialf(GL_FRONT, GL_SHININESS, shininessMaterial);
		glTranslated(targetCenter_x,targetCenter_y,targetCenter_z);
		gluSphere(targetBall, targetRadius, 64, 64);
		gluDeleteQuadric(targetBall);
		glPopMatrix();
		
		 //2. Draw cue ball
		glColor3f(0.8,0.0,0.0);
		glPushMatrix();
		glLoadIdentity();
		GLUquadricObj* cueBall = gluNewQuadric();
		gluQuadricDrawStyle(cueBall, GLU_FILL);
		glMaterialfv(GL_FRONT, GL_SPECULAR, specularMaterial);
		glMaterialf(GL_FRONT, GL_SHININESS, shininessMaterial);
		glTranslated(cueCenter_x,cueCenter_y,cueCenter_z);
		gluSphere(cueBall, cueRadius, 64, 64);
		gluDeleteQuadric(cueBall);
		glPopMatrix();
		
		
		// 3. Draw Comparison ball
		glColor3f(0.8,0.0,0.0);
		glPushMatrix();
		glLoadIdentity();
		GLUquadricObj* compBall = gluNewQuadric();
		gluQuadricDrawStyle(compBall, GLU_FILL);
		glMaterialfv(GL_FRONT, GL_SPECULAR, specularMaterial);
		glMaterialf(GL_FRONT, GL_SHININESS, shininessMaterial);
		glTranslated(compCenter_x,targetCenter_y,compCenter_z);
		gluSphere(compBall, targetRadius, 64, 64);
		gluDeleteQuadric(compBall);
		glPopMatrix();

		// 4. Draw Contact Point
		glColor3f(0.0,0.0,0.0);
		GLUquadricObj* quadratic;
		quadratic = gluNewQuadric();
		gluQuadricNormals(quadratic, GLU_SMOOTH);
		glPushMatrix();
		glMaterialfv(GL_FRONT, GL_SPECULAR, specularMaterial);
		glMaterialf(GL_FRONT, GL_SHININESS, shininessMaterial);
		glTranslatef(cPointX,cPointY,cPointZ);
		glRotatef(pointRotation,0.0,1.0,0.0);
		gluDisk(quadratic,0,2,32,32);
		glPopMatrix(); 
	}
	
	// draw response line
	/*if(( contactOccurred && elapsed>=(timeOfContact+occlusion+ responseDelay))) //  after display period
		
	{
		glLoadIdentity();
		glColor3f(1.0f,0.0f,0.0f);
		glBegin(GL_LINES);
		glVertex3f(linePosition, -50, displayDepth-goalDepth);
		glVertex3f(linePosition, -100, displayDepth-goalDepth);
		glEnd();
	}// is parameterised by lineposition variable
	*/

	// 3. Draw fingertip
	if(!cueBallStruck){
		glPushMatrix();
		glLoadIdentity();
		glColor3f(0.9f,0.0f,0.0f);
		GLUquadricObj* qobj = gluNewQuadric();
		gluQuadricDrawStyle(qobj, GLU_FILL);
		glMaterialfv(GL_FRONT, GL_SPECULAR, specularMaterial);
		glMaterialf(GL_FRONT, GL_SHININESS, shininessMaterial);
		glTranslated(indXNow,indYNow,indZNow); 
		gluSphere(qobj, 2, 6, 6);
		gluDeleteQuadric(qobj);
		glPopMatrix();
	}
}


// called at the beginning of every trial
void initTrial()
{
	// initializing all variables
	frameN=0;
	cueBallStruck = false;
	cueVelSet = false;
	contactOccurred = false;
	timeOfContact = 999999999;
	response = -1;
	string subjectName = parameters.find("SubjectName");
	string trialFileName = experiment_directory_desktop + subjectName + "/" + subjectName + "_trial" + stringify<double>(trialNumber) + ".txt";
	trialFile.open(trialFileName.c_str());
	trialFile << fixed << trialFile_headers << endl;

	occlusion = trial.getCurrent().first["Occlusion"];
	compPosition = trial.getCurrent().first["compPosition"];
	int aimingAngle_index = trial.getCurrent().first["AimingAngle"];
	int deflectionAngle_index = trial.getCurrent().first["DeflectionAngle"];


	compAngle =  trial.getCurrent().second->getCurrentStaircase()->getState();
	if (compAngle <0){
	compVel_x_temp = -1;
	compVel_z_temp = 1/tan(compAngle*DEG2RAD);
	}
	else if (compAngle > 0){
	compVel_x_temp = 1;
	compVel_z_temp = -1/tan(compAngle*DEG2RAD);
	}
	else {
	compVel_x_temp = 0;
	compVel_z_temp = -1;
	}
	
	cueCenter_x = 0;
	cueCenter_y = -75;
	cueCenter_z = displayDepth + distanceCue2TargetZ; 
	
	targetVel_x = 0;
	targetVel_y = 0;
	targetVel_z = 0;
	
	// Straight Ahead at viewing distance
	targetCenter_x = 0;
	targetCenter_y = -75;
	targetCenter_z = displayDepth; 

	compCenter_x  = compPosition;
	compCenter_z = displayDepth;
	

	
	float goalAngle;
	switch(aimingAngle_index){ 
		case 0: //Left
			aimingAngle = -10;
			cueVel_x_temp = -1;
			cueVel_z_temp = 1/tan(aimingAngle*DEG2RAD);
			indXNow = 35.27;
			indYNow = -75;
			indZNow = -200;
			movementX = -0.3527;
			movementZ = -1;
			movementY = 0; 
			lineVelMagnitude = sqrt(pow(movementX,2) + pow(movementZ,2)); //magnitude of vector normalised 
			movementX = (movementX/lineVelMagnitude)*speed;
			movementZ = (movementZ/lineVelMagnitude)*speed;
			cPointX = -6.22;
			cPointY = targetCenter_y;
			cPointZ = (displayDepth + distanceCue2TargetZ)- 85.25;
			pointRotation = -22.86;
			break;
		
		case 1: // Right
			aimingAngle = 10;
			cueVel_x_temp = 1;
			cueVel_z_temp = -1/tan(aimingAngle*DEG2RAD);
			indXNow = -35.27;
			indYNow = -75;
			indZNow = -200;
			movementX = 0.3527;
			movementZ = -1;
			movementY = 0;
			lineVelMagnitude = sqrt(pow(movementX,2) + pow(movementZ,2)); //magnitude of vector normalised 
			movementX = (movementX/lineVelMagnitude)*speed;
			movementZ = (movementZ/lineVelMagnitude)*speed;
			cPointX = 6.22;
			cPointY = targetCenter_y;
			cPointZ = (displayDepth + distanceCue2TargetZ) - 85.25;
			pointRotation = 22.86;
			break;
	}
	switch(deflectionAngle_index){
		case 0: //Left
			deflectionAngle= -22.86;
			targetVel_x_temp = -1;
			targetVel_z_temp = 1/tan(deflectionAngle*DEG2RAD);
			break;					
		case 1: //Straight Ahead
			deflectionAngle= 0;
			targetVel_x_temp = 0;
			targetVel_z_temp = -1;
			break;

		case 2:  //Right
			deflectionAngle= 22.86;
			targetVel_x_temp = 1;
			targetVel_z_temp = -1/tan(deflectionAngle*DEG2RAD);
			break;
	}


	
	initProjectionScreen(displayDepth);
	
	// roll on
	drawGLScene();
	timer.start();
}

// This function handles the transition from the end of one trial to the beginning of the next.
void advanceTrial() {
	
	trialFile << fixed <<
	parameters.find("SubjectName") << "\t" <<		//subjName
	trialNumber << "\t" <<							//trialN
	activePhase << "\t" <<
	occlusion << "\t" <<
	aimingAngle << "\t" <<
	compPosition <<"\t"<<
	speed << "\t" <<
	deflectionAngle << "\t" <<
	response << "\t" << // true or false (to the left or to the right)
	timeOfContact << "\t" <<
	elapsed << "\t" <<
	frameN << "\t" <<
	cueBallStruck << "\t" <<
	cueVelSet << "\t" <<
	contactOccurred << "\t" <<
	cueCenter_x << "\t" <<
	cueCenter_y << "\t" <<
	cueCenter_z << "\t" <<
	targetCenter_x << "\t" <<
	targetCenter_y << "\t" <<
	targetCenter_z << "\t" <<
	ind.transpose() << "\t" <<
	compAngle << endl;

	if(trialFile.is_open())
		trialFile.close();

	if(!trial.isEmpty()){
		trial.next(response);
		trialNumber++;
		initTrial();
	}else{
		finished=true;
	}
}

void idle() {

	elapsed = timer.getElapsedTimeInMilliSec();

	// get new marker positions from optotrak
	updateTheMarkers();

	// eye coordinates
	eyeRight = Vector3d(interoculardistance/2,0,0);//0
	eyeLeft = Vector3d(-interoculardistance/2,0,0);//0

	// Write to trialFile once calibration is over
	if (fingersCalibrated) // write every frame if grasping
	{
		trialFile << fixed <<
		parameters.find("SubjectName") << "\t" <<		//subjName
		trialNumber << "\t" <<							//trialN
		activePhase << "\t" <<
		occlusion << "\t" <<
		aimingAngle << "\t" <<
		compPosition <<"\t"<<
		speed << "\t" <<
		deflectionAngle << "\t" <<
		response << "\t" << 
		timeOfContact << "\t" <<
		elapsed << "\t" <<
		frameN << "\t" <<
		cueBallStruck << "\t" <<
		cueVelSet << "\t" <<
		contactOccurred << "\t" <<
		cueCenter_x << "\t" <<
		cueCenter_y << "\t" <<
		cueCenter_z << "\t" <<
		targetCenter_x << "\t" <<
		targetCenter_y << "\t" <<
		targetCenter_z << "\t" <<
		ind.transpose() << "\t" <<
		compAngle << endl;
	}
}



/*** Online operations ***/
void online_apparatus_alignment()
{
	if(visibleInfo){
		// mirror alignment check
		mirrorAlignment = asin(
				abs((markers.at(mirror1).p.z()-markers.at(mirror2).p.z()))/
				sqrt(
				pow(markers.at(mirror1).p.x()-markers.at(mirror2).p.x(), 2) +
				pow(markers.at(mirror1).p.z()-markers.at(mirror2).p.z(), 2)
				)
				)*180/M_PI;

		// screen Y alignment check
		screenAlignmentY = asin(
				abs((markers.at(screen1).p.y()-markers.at(screen3).p.y()))/
				sqrt(
				pow(markers.at(screen1).p.x()-markers.at(screen3).p.x(), 2) +
				pow(markers.at(screen1).p.y()-markers.at(screen3).p.y(), 2)
				)
				)*180/M_PI;

		// screen Z alignment check
		screenAlignmentZ = asin(
				abs(markers.at(screen1).p.z()-markers.at(screen2).p.z())/
				sqrt(
				pow(markers.at(screen1).p.x()-markers.at(screen2).p.x(), 2) +
				pow(markers.at(screen1).p.z()-markers.at(screen2).p.z(), 2)
				)
				)*180/M_PI*
				abs(markers.at(screen1).p.x()-markers.at(screen2).p.x())/
				(markers.at(screen1).p.x()-markers.at(screen2).p.x());
	}
}

void online_fingers()
{
	// Visibility check
	allVisibleIndex = isVisible(markers.at(ind1).p) && isVisible(markers.at(ind2).p) && isVisible(markers.at(ind3).p);
	//allVisibleThumb = isVisible(markers.at(thu1).p) && isVisible(markers.at(thu2).p) && isVisible(markers.at(thu3).p);
	allVisibleFingers = allVisibleIndex;

	// fingers coordinates, fingersOccluded and framesOccluded
	if ( allVisibleFingers )
	{
		indexCoords.update(markers.at(ind1).p, markers.at(ind2).p, markers.at(ind3).p );
		//thumbCoords.update(markers.at(thu1).p, markers.at(thu2).p, markers.at(thu3).p );
		//indexJointCoords.update(markers.at(ind1).p, markers.at(ind2).p, markers.at(ind3).p );
		//thumbJointCoords.update(markers.at(thu1).p, markers.at(thu2).p, markers.at(thu3).p );
	}

	if(fingersCalibrated)
	{
		// index coordinates
		if(allVisibleIndex){
			ind = indexCoords.getP1();
			//if(fingerCalibrationDone>=3)
			//	indJoint = indexJointCoords.getP1();
		}
		// thumb coordinates
		//if(allVisibleThumb){
			//thm = thumbCoords.getP1();
			//if(fingerCalibrationDone>=3)
			//	thmJoint = thumbJointCoords.getP1();
		//}
	}
}

void online_trial()
{

	// while the experiment is running
	if (fingersCalibrated && !finished)
	{
		// Advance frame number
		frameN++;

		// in the active phase, get the marker-based index finger position
		if(activePhase){
			indXNow = ind.x();
			indYNow = ind.y();
			indZNow = ind.z();
		// in the passive phase, get the remembered trajectories XX
		}else{
			if(!cueBallStruck ){ // (only when the cueBall isn't yet struck)
					indXNow = indXNow + movementX;
					indYNow = indYNow + movementY ;
					indZNow = indZNow + movementZ ;
								
			}
		}

		// test if behind virtual wall at 280 mm
		distanceToStart = -280 - indZNow;
		handAtStart = distanceToStart<0;

		// test for cue ball struck
		if(!cueBallStruck){
			distanceToCueBall = sqrt(pow(indXNow-cueCenter_x,2)+pow(indYNow-cueCenter_y,2)+pow(indZNow-cueCenter_z,2));
			cueBallStruck =  distanceToCueBall <= cueRadius;
		}

		// set the cue velocity after striking, this only happens once per trial.
		if(cueBallStruck && !cueVelSet){
			double cueVelMagnitude = sqrt(pow(cueVel_x_temp,2) + pow(cueVel_z_temp,2)); //magnitude of vector normalised 
			cueVel_x = (cueVel_x_temp/cueVelMagnitude)*speed;
			cueVel_z = (cueVel_z_temp/cueVelMagnitude)*speed;
			cueVelSet = true;
		}
		
		// update ball positions
		if(cueBallStruck && !contactOccurred){
			cueCenter_x += cueVel_x;
			cueCenter_y += cueVel_y;
			cueCenter_z += cueVel_z;
		}
		
		if(contactOccurred){
			targetCenter_x += targetVel_x;
			targetCenter_y += targetVel_y;
			targetCenter_z += targetVel_z;
			compCenter_x += compVel_x;
			compCenter_z += compVel_z;
			if (abs(targetCenter_z +500) < 10) {
				cout<< targetCenter_z + 500 << endl;
				cout<< elapsed - timeOfContact << endl;
			}
		}
		
		// And check for the collision
		if(!contactOccurred){
			float distanceBetween_x = targetCenter_x -cueCenter_x; 
			float distanceBetween_y = targetCenter_y -cueCenter_y ;
			float distanceBetween_z = targetCenter_z -cueCenter_z;
			distanceBetween = sqrt(pow(distanceBetween_x, 2) + pow(distanceBetween_y, 2) + pow(distanceBetween_z, 2));
			
			if ((distanceBetween <= cueRadius+targetRadius) ){ 
			
				// when the collision occurs
				contactOccurred = true;
				timeOfContact = elapsed;

				// freeze the cue ball
				cueVel_x = 0;
				cueVel_y = 0;
				cueVel_z = 0;
	
				// normalize the target velocity and multiply by speed
				vecLength_unnorm = sqrt(pow(targetVel_x_temp, 2) + pow(targetVel_z_temp, 2));
				targetVel_x = speed * (targetVel_x_temp/vecLength_unnorm);
				targetVel_z = speed * (targetVel_z_temp/vecLength_unnorm);


				//comparison ball
				CompVecLength_unnorm = sqrt(pow(compVel_x_temp, 2) + pow(compVel_z_temp, 2));
				compVel_x = speed2 * (compVel_x_temp/CompVecLength_unnorm);
				compVel_z = speed2 * (compVel_z_temp/CompVecLength_unnorm);
			}
		}
	}
}


///////////////////////////////////////////////////////////
/////// USUALLY DON'T NEED TO EDIT THESE FUNCTIONS ////////
///////////////////////////////////////////////////////////

void updateTheMarkers()
{
	optotrak.updateMarkers();
	markers = optotrak.getAllMarkers();
}

void initVariables() 
{
	trial.init(parameters);
	interoculardistance = str2num<double>(parameters.find("IOD"));
}

void update(int value)
{
    glutPostRedisplay();
    glutTimerFunc(TIMER_MS, update, 0);
}

void handleResize(int w, int h)
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glViewport(0,0,SCREEN_WIDTH, SCREEN_HEIGHT);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
}

void initProjectionScreen(double _focalDist, const Affine3d &_transformation, bool synchronous)
{
	focalDistance = _focalDist;	
    screen.setWidthHeight(SCREEN_HIGH_SIZE*SCREEN_WIDTH/SCREEN_HEIGHT, SCREEN_HIGH_SIZE);//(SCREEN_WIDE_SIZE, SCREEN_WIDE_SIZE*SCREEN_HEIGHT/SCREEN_WIDTH);
    screen.setOffset(alignmentX,alignmentY);
    screen.setFocalDistance(_focalDist);
    screen.transform(_transformation);
    cam.init(screen);
	if ( synchronous )
		moveScreenAbsolute(_focalDist,homeFocalDistance,4500);
	else
		moveScreenAbsoluteAsynchronous(_focalDist,homeFocalDistance,4500);
}

void initRendering()
{   
	// Clear buffers
	glClearColor(0.0,0.0,0.0,1.0);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    /* Set depth buffer clear value */
    glClearDepth(1.0);

    /* Enable depth test */
    glEnable(GL_DEPTH_TEST);

	// Not sure...
	//glEnable(GL_CULL_FACE);

    /* Set depth function */
    glDepthFunc(GL_LEQUAL);

	// Nice perspective calculations
	glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

	// Set up the lighting
	glShadeModel(GL_SMOOTH);
	glEnable(GL_NORMALIZE);
	glEnable(GL_LIGHTING);
	glEnable(GL_COLOR_MATERIAL);

	glLightfv(GL_LIGHT1, GL_AMBIENT, LightAmbient);
	glLightfv(GL_LIGHT1, GL_DIFFUSE, LightDiffuse);
	glLightfv(GL_LIGHT1, GL_SPECULAR, LightSpecular);
	glLightfv(GL_LIGHT1, GL_POSITION, LightPosition);
	glLightf(GL_LIGHT1, GL_CONSTANT_ATTENUATION, 0.5f);
	glEnable(GL_LIGHT1);
	glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_TRUE);


	// Clean modelview matrix to start
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
}

void initMotors()
{
	homeEverything(6000,4000);
}

void initOptotrak()
{
    optotrak.setTranslation(calibration);

    if ( optotrak.init(LastAlignedFile, OPTO_NUM_MARKERS, OPTO_FRAMERATE, OPTO_MARKER_FREQ, OPTO_DUTY_CYCLE,OPTO_VOLTAGE) != 0)
    {   cerr << "Something during Optotrak initialization failed, press ENTER to continue. A error log has been generated, look \"opto.err\" in this folder" << endl;
        cin.ignore(1E6,'\n');
        exit(0);
    }

    // Read 10 frames of coordinates and fill the markers vector
    for (int i=0; i<10; i++)
    {
        updateTheMarkers();
    }
}

void cleanup()
{
	// Stop the optotrak
	optotrak.stopCollection();
}

void beepOk(int tone)
{
	#ifndef SIMULATION
		switch(tone)
		{
		case 0:
	    // Remember to put double slash \\ to specify directories!!!
	    PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\beep-1.wav", 
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 1:
	    PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\calibrate.wav", 
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 2:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\beep-8.wav", 
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 3:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\beep-reject.wav", 
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 4:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\beep-twoBlips.wav", 
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 7:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\spoken-left.wav", 
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 8:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\spoken-right.wav", 
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 9:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\spoken-home.wav", 
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 10:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\spoken-grasp.wav", 
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 11:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\spoken-marker.wav",
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 12:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\spoken-estimate.wav",
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 13:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\beep-8_lowpass.wav",
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 14:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\beep-8_double.wav",
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 15:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\beep-rising.wav",
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 16:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\beep-falling.wav",
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 17:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\beep-highBubblePop.wav",
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 18:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\beep-lowBubblePop.wav",
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 19:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\spoken-play.wav",
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		case 20:
		PlaySound((LPCSTR) "C:\\cygwin\\home\\visionlab\\workspace\\cncsvision\\data\\beep\\spoken-watch.wav",
			NULL, SND_FILENAME | SND_ASYNC);
		break;
		}
	#endif
	return;
}

///////////////////////////////////////////////////////////
////////////////////// MAIN FUNCTION //////////////////////
///////////////////////////////////////////////////////////

int main(int argc, char*argv[])
{
	mathcommon::randomizeStart();
	
	// Initializes the optotrak and starts the collection of points in background
    initMotors();
	initOptotrak();

    glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH | GLUT_STEREO);
	glutGameModeString(GAME_MODE_STRING);
	glutEnterGameMode();
	//glutFullScreen();
	
    initRendering();
	initStreams(); // parameters file is loaded
	
	initVariables(); // staircases are built
	
    glutDisplayFunc(drawGLScene);
    glutKeyboardFunc(handleKeypress);
    glutReshapeFunc(handleResize);
    glutIdleFunc(idle);
    glutTimerFunc(TIMER_MS, update, 0);
    glutSetCursor(GLUT_CURSOR_NONE);

	//boost::thread initVariablesThread(&initVariables);

    glutMainLoop();

	homeEverything(6000,4000);
    cleanup();
    return 0;
}

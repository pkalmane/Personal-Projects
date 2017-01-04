'------------------------------------------------------------------------------
' user interface
'------------------------------------------------------------------------------
%USEMACROS = 1
#INCLUDE ONCE "Globals.inc"
#INCLUDE ONCE "cgdiplus.inc"

'#INCLUDE ONCE "Camera.inc"
'--------------------------------------------------------------------
DECLARE FUNCTION ReadSUTDigitalOutput1 LIB "AHTSDLL.DLL" ALIAS "ReadSUTDigitalOutput" (BYVAL iOutputNumber AS LONG) AS LONG
DECLARE FUNCTION WriteSUTDigitalInput1 LIB "AHTSDLL.DLL" ALIAS "WriteSUTDigitalInput" (BYVAL iInputNumber AS LONG,iValue AS LONG) AS LONG
'--------------------------------------------------------------------
DECLARE FUNCTION CameraON LIB "CameraDLL" ALIAS "CameraON" (BYVAL hCap AS LONG) AS LONG
DECLARE FUNCTION CameraOFF LIB "CameraDLL" ALIAS "CameraOFF" (BYVAL hCap AS LONG) AS LONG
DECLARE FUNCTION SetCameraWindow LIB "CameraDLL" ALIAS "SetCameraWindow" (BYVAL hCap AS LONG) AS LONG
DECLARE FUNCTION SaveImage_GDIP  LIB "CameraDLL" ALIAS "SaveImage_GDIP" (BYREF sJpgFileName AS STRING, BYVAL targetHandle AS DWORD, BYVAL targetID AS LONG) AS LONG
DECLARE FUNCTION Save1Image_GDIP  LIB "CameraDLL" ALIAS "SaveImage_GDIP" (BYREF sJpgFileName AS STRING, BYVAL targetHandle AS DWORD, BYVAL targetID AS LONG) AS LONG
DECLARE FUNCTION Save2Image_GDIP ( sJpgFileName AS STRING, BYVAL targetHandle AS DWORD, BYVAL targetID AS LONG)  AS LONG
DECLARE FUNCTION  CaptureImageFromDesktop LIB "CameraDLL" ALIAS "SaveImageFromDesktop"(sFIlename AS STRING , hDLG AS DWORD,lCapW AS LONG, lCapH AS LONG )AS LONG

' functions exported by AVICAP32.DLL
DECLARE FUNCTION capCreateCaptureWindow   LIB "AVICAP32.DLL" ALIAS "capCreateCaptureWindowA"  _
        (lpszWindowName AS ASCIIZ, _
        BYVAL dwStyle AS DWORD, _
        BYVAL x AS LONG,  BYVAL y AS LONG, BYVAL nWidth AS LONG, BYVAL nHeight AS LONG, _
        BYVAL  hWnd AS LONG,  _
        BYVAL nID AS LONG) AS LONG
DECLARE FUNCTION capGetDriverDescription  LIB "AVICAP32.DLL" ALIAS "capGetDriverDescriptionA" _
        (BYVAL DriverIndex  AS LONG, _
        lpszName AS ASCIIZ, _
        BYVAL szNameLen AS LONG, _
        lpszVersion  AS ASCIIZ, _
        BYVAL szVersionLen AS LONG) AS LONG

'------------------------------------------------------------------------------
' Equates
'------------------------------------------------------------------------------


'------------------------------------------------------------------------------
FUNCTION OperateFunction() AS LONG
    MSGBOX "Adjust the Camera view such that Lactometer image is seen in the centre of the picture window",,$AppTitle
    ShowoperateDIALOG()
END FUNCTION
'-----------------------------------------------------------------------------




'------------------------------------------------------------------------------
' Global data variables
'------------------------------------------------------------------------------
GLOBAL gIsRecModified  AS LONG
GLOBAL gIsFileModified AS LONG
GLOBAL gCurrentEntry   AS LONG
GLOBAL gMaxEntries     AS LONG
GLOBAL gsFindText      AS STRING

'------------------------------------------------------------------------------
' Main entry point for the application
'------------------------------------------------------------------------------
FUNCTION ShowOperateDIALOG () AS LONG

    REGISTER x       AS LONG
    LOCAL    hDlg    AS DWORD
    LOCAL    hMenu   AS DWORD
    LOCAL    Result  AS LONG
    LOCAL    hPopup1 AS DWORD
    LOCAL    hPopup2 AS DWORD
    LOCAL    nFile   AS LONG

    DIALOG FONT "Tahoma"
    MOUSEPTR 11

    ' Create our primary GUI
    DIALOG NEW 0, $AppTitle+ " - Operations", 10,10, 600, 320, _
        %DS_CENTER OR %WS_CAPTION OR %WS_SYSMENU, 0 TO hDlg

    ' Abort if the dialog could not be created
    IF hDlg = 0 THEN EXIT FUNCTION  ' Error occurred

    DIALOG SET COLOR hDlg, -1&, RGB(255,255,222)

    ' Now we create our controls.  We create the static control immediately before the
    ' real control so that the keyboard accelerators switch focus to the real control.


    CONTROL ADD LABEL,    hDlg, 20,  "USER ID NO.", 10,  20,  80,  10
    CONTROL ADD TEXTBOX,  hDlg, %ID_NEW_RFID_TEXTBOX, "1234567890", 100,   20,  80,  10,%SS_CENTER

    CONTROL ADD LABEL,    hDlg, 21,       "First Na&me",   10,  40,  80,   10
    CONTROL ADD TEXTBOX,  hDlg, %ID_FIRSTNAME,    "Farmer1",  100,  40, 90,  12


    CONTROL ADD LABEL,    hDlg, 22,           "La&st", 10,  60,  80,   10
    CONTROL ADD TEXTBOX,  hDlg, %ID_LASTNAME,   "Lastname", 100,  60, 90,  12

    CONTROL ADD LABEL,    hDlg, 23,           "Mobile.no",  10, 80,  60,   10
    CONTROL ADD TEXTBOX,  hDlg, %ID_MOBILE,       "8904657386",  100, 80, 80,  12


    CONTROL ADD LABEL,    hDlg, 24,  "MILK TEMPERATURE", 10,   100,  80,  10
    CONTROL ADD TEXTBOX,  hDlg, %ID_MILK_TEMPERATURE_TEXTBOX, "25.0", 100, 100,  80, 12

    CONTROL ADD LABEL,    hDlg, 25,  "LACTOMETER READING.", 10,  120,  80,  10
    CONTROL ADD TEXTBOX,  hDlg, %ID_LACTO_READING_TEXTBOX, "44", 100, 120,  80,  12

    CONTROL ADD LABEL,    hDlg, 26,  "PRE DELIVERY WEIGHT", 10,  140,  80,  10
    CONTROL ADD TEXTBOX,  hDlg, %ID_PRE_WEIGHT_TEXTBOX, "0000", 100, 140,  80,  12

    CONTROL ADD LABEL,    hDlg, 27,  "POST DELIVERY WEIGHT", 10, 160,  85,  10
    CONTROL ADD TEXTBOX,  hDlg, %ID_POST_WEIGHT_TEXTBOX, "20.00", 100, 160,  80,  12

    CONTROL ADD  FRAME, hDlg,  28,  "",5,5,200,225

    CONTROL ADD TEXTBOX,  hDlg, %ID_MODEM_TEXTBOX,"ModemData",10,235,200,80, _
                    %WS_TABSTOP OR %WS_BORDER OR %ES_LEFT OR %ES_MULTILINE OR %WS_VSCROLL,%WS_EX_CLIENTEDGE OR %WS_EX_LEFT

    CONTROL ADD GRAPHIC , hDlg,%ID_PICTURE,"Camera",lGWX ,lGWY ,lGWW ,lGWH
    ' Color the LABEL controls (foreground = default, background = transparent)
    FOR x = 19 TO 34
        CONTROL SET COLOR hDlg, x, -1, -2
    NEXT x
        CONTROL SET COLOR hDlg, %ID_MODEM_TEXTBOX, -1, -2

    ' Add the command buttons
    CONTROL ADD BUTTON,   hDlg, %ID_PRIOR, "LACTO",  10, 200,  30,  14 CALL xMeasureCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_WEIGHT, "WEIGHT",  45, 200,  30,  14 CALL xWeightCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_NEXT,   "&DONE",  80, 200,  30,  14 CALL xDoneCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_CLEAR,  "&CLEAR",  120, 200,  30,  14 CALL xClearOperateCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_FIND,      "E&XIT",  160, 200,  30,  14 CALL xExitOperateCallBack


    sModemCommunication=""

    lDestinationTextBox=%id_NEW_RFID_TEXTBOX
    hDestinationFormHandle=hDlg

    gOperationFlag=1             ' when this is set, thread will check if new rfid has come
    ' Start up our GUI, and run until the user quits
    DIALOG SHOW MODAL hDlg, CALL xOperateCallBack TO Result

    '----------------------------------------------------------------

END FUNCTION


'------------------------------------------------------------------------------
' Main dialog callback
'------------------------------------------------------------------------------
CALLBACK FUNCTION xOperateCallBack

    LOCAL Dummy AS LONG
    LOCAL hDlg  , hCap ,hDC AS DWORD
    LOCAL x1,y1,x2,y2,x3,y3 AS LONG



'    LOCAL d1 AS DateInterface
'    LOCAL s1 AS STRING
'    LET d1=CLASS "clsDate"
'    s1=d1.sddmmyyyy

    SELECT CASE CB.MSG



    CASE %WM_INITDIALOG
'        x1=lGWY:lGWY=lGWX:lGWX=x1:x1=lGWW:lGWW=lGWH:lGWH=x1
       DIALOG UNITS CBHNDL,lGWX,lGWY TO PIXELS x1,y1
        DIALOG UNITS CBHNDL,lGWW,lGWH TO PIXELS x2,y2
'        msgbox str$(lGWX)+ STR$(lGWY)+STR$(lGWW)+STR$(lGWH)+"="+str$(x1)+STR$(y1)+STR$(x2)+STR$(y2)


        CONTROL HANDLE CBHNDL, %ID_PICTURE TO hPictureHandle


            hCap = capCreateCaptureWindow("Test CapWindow", BYVAL (%WS_CHILD OR %WS_VISIBLE), BYVAL x1, BYVAL y1, BYVAL x2, BYVAL y2, BYVAL CB.HNDL, BYVAL 1001)
            hCameraWindowHandle=hCap
            CameraOn(hCameraWindowHandle)
            SetCameraWindow(hCameraWindowHandle)
            MOUSEPTR 1
            'GRAPHIC ATTACH hCameraWindowHandle,0
            GRAPHIC ATTACH hDEstinationFormHandle,%ID_PICTURE
            GRAPHIC GET DC TO hDC
            GRAPHIC COPY hCameraWindowHandle,hDC
'            GRAPHIC getDC TO ghDC
            lLinkFLag=0


    CASE %WM_COMMAND
        IF CB.CTLMSG = %EN_UPDATE THEN       ' *Any* edit control was changed
'            ' If this is the first "change" message, we need to update the
'            ' controls in the dialog box.
'            IF ISFALSE gIsRecModified THEN
'                gIsRecModified = %TRUE
'                xSetButtonStatus CBHNDL
'            END IF
        END IF

        ' Now process any other %WM_COMMAND message from controls that don't
        ' have their own callback functions.
        SELECT CASE CB.CTL

'        CASE %id_Menu_Help
'            Dummy = SHELL("WINHELP.EXE", 1)
'
'        CASE %id_Menu_About
'            ' About dialog
'            DIALOG NEW CB.HNDL, "New Data", 0, 0, 170, 60, _
'                        %DS_MODALFRAME OR %WS_CAPTION OR %WS_SYSMENU OR %DS_CENTER TO hDlg
'            DIALOG SET COLOR hDlg, -1&, %BLUE
'            IF hDlg = 0 THEN EXIT FUNCTION  ' Error occurred
'            CONTROL ADD LABEL, hDlg, -1, $AppTitle , _
'                        15, 10, 130, 34, %SS_CENTER
'            CONTROL SET COLOR hDlg, -1, %WHITE, %BLUE
'            CONTROL ADD BUTTON, hDlg, 1, "&Ok", 60, 42, 40, 14 CALL xOkCallBack
'            BEEP
'            DIALOG SHOW MODAL hDlg
        END SELECT

    CASE %WM_DESTROY
        gOperationFlag=0

    END SELECT

END FUNCTION

'------------------------------------------------------------------------------
'sub  UpdateOperationScreen(sID as string)
'------------------------------------------------------------------------------
SUB  UpdateOperationScreen(sID1 AS STRING)
LOCAL s1,s2,s3 AS STRING
LOCAL l1,l2,l3 AS LONG

l1=CheckifDuplicate(sID1,lFarmerCOunt)
IF l1>0  THEN ' record found

    s1=FarmerData(l1).sFirstname
    s2=FarmerData(l1).sLastName
    s3=FarmerData(l1).sMobile

ELSE
    s1="No name":
    s2="No name":
    s3="No Mobile number"


END IF
CONTROL SET TEXT hDestinationFormHandle,%ID_FIRSTNAME,s1
CONTROL SET TEXT hDestinationFormHandle,%ID_LASTNAME,s2
CONTROL SET TEXT hDestinationFormHandle,%ID_MOBILE,s3

END SUB


'------------------------------------------------------------------------------
' PRIOR/NEXT Button callback function
'------------------------------------------------------------------------------




'------------------------------------------------------------------------------
' "Generic" OK callback function for "messagebox" style DDT dialogs
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
CALLBACK FUNCTION xWeightCallBack
LOCAL s1 AS STRING
's1=INPUTBOX$( "Enter current weighing scale reading (in Kgs)",,"0.0")
s1=CWindow_InputBox (hDestinationFormHandle,20,20,"Enter current weighing scale reading ","Enter weight (in Kgs)","10.00")
CONTROL SET TEXT hDestinationFormHandle,%ID_POST_WEIGHT_TEXTBOX,s1

END FUNCTION

'------------------------------------------------------------------------------
CALLBACK FUNCTION xClearOperateCallBack
LOCAL s1 AS STRING
s1=""
CONTROL SET TEXT hDestinationFormHandle,%ID_NEW_RFID_TEXTBOX,s1
CONTROL SET TEXT hDestinationFormHandle,%ID_FIRSTNAME,s1
CONTROL SET TEXT hDestinationFormHandle,%ID_LASTNAME,s1
CONTROL SET TEXT hDestinationFormHandle,%ID_MOBILE,s1
CONTROL SET TEXT hDestinationFormHandle,%ID_MILK_TEMPERATURE_TEXTBOX,s1
CONTROL SET TEXT hDestinationFormHandle,%ID_LACTO_READING_TEXTBOX,s1
CONTROL SET TEXT hDestinationFormHandle,%ID_PRE_WEIGHT_TEXTBOX,s1
CONTROL SET TEXT hDestinationFormHandle,%ID_POST_WEIGHT_TEXTBOX,s1

'SendSMS("9164682295","ABCD"+time$)

END FUNCTION

'------------------------------------------------------------------------------
CALLBACK FUNCTION xMeasureCallBack
LOCAL s1 ,sResult AS STRING
LOCAL lfile AS LONG

CONTROL GET TEXT hDestinationFormHandle,%ID_NEW_RFID_TEXTBOX TO s1
    s1=trimRFID(s1)


GdiPlusSaveImageToFileNew

gsRFID=TRIM$(UCASE$(s1))
lLinkFlag=0
InterProcessCommunicationLink1("Ready")
MSGBOX "Waiting for DIP module to complete analysis. Switch to DIP module screen and complete the operation",,$AppTitle

'lfile=FREEFILE
'OPEN $InterProcessResult FOR INPUT AS #lfile
'LINE INPUT #lfile,sResult
'CLOSE #lfile
'
'KILL $InterProcessResult
'
'CONTROL SET TEXT hDestinationFormHandle,%ID_LACTO_READING_TEXTBOX,TRIM$(sResult)

END FUNCTION

'------------------------------------------------------------------------------
'------------------------------------------------------------------------------
CALLBACK FUNCTION xDoneCallBack
LOCAL s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12 , sBitmap , sResult AS STRING
LOCAL d1,d2 ,d3 AS SINGLE
LOCAL TempTransaction AS TypTransactionData
LOCAL hDC ,hBMP1, hBMP2 AS DWORD
LOCAL l1 ,lfile AS LONG

LOCAL dt1 AS DateInterface

l1=InterProcessCommunicationLink2()
IF l1=0 THEN
    IF lLinkFLag=0 THEN
        MSGBOX "LactoMeter Reading not yet complete",,$AppTitle
        EXIT FUNCTION
    ELSE
        lLinkFlag=0
    END IF
ELSE
    IF lLinkFlag=0 THEN
        lLinkFLag=1
        MSGBOX "LactoMeter Reading recognised.Click on DONE button again to record transaction",,$AppTitle

        EXIT FUNCTION
    ELSE

    END IF

END IF

lfile=FREEFILE
OPEN $InterProcessResult FOR INPUT AS #lfile
LINE INPUT #lfile,sResult
CLOSE #lfile

KILL $InterProcessResult

sResult=ACIR(sResult)   'Future use for calibration
CONTROL SET TEXT hDestinationFormHandle,%ID_LACTO_READING_TEXTBOX,TRIM$(sResult)

MSGBOX "LactoMeter Reading updated",,$AppTitle
LET dt1=CLASS "clsDate"
CONTROL GET TEXT hDestinationFormHandle,%ID_NEW_RFID_TEXTBOX TO s1
    s1=trimRFID(s1)

CONTROL GET TEXT hDestinationFormHandle,%ID_FIRSTNAME TO s2
CONTROL GET TEXT hDestinationFormHandle,%ID_LASTNAME TO s3
CONTROL GET TEXT hDestinationFormHandle,%ID_MOBILE TO s4
CONTROL GET TEXT hDestinationFormHandle,%ID_MILK_TEMPERATURE_TEXTBOX TO s5
CONTROL GET TEXT hDestinationFormHandle,%ID_LACTO_READING_TEXTBOX TO s6
CONTROL GET TEXT hDestinationFormHandle,%ID_PRE_WEIGHT_TEXTBOX TO s7
CONTROL GET TEXT hDestinationFormHandle,%ID_POST_WEIGHT_TEXTBOX TO s8

IF LEN(s1)=0 THEN
    MSGBOX "Improper ID. Record not accepted",,$AppTitle
    EXIT FUNCTION
END IF

IF LEN(s2)=0 THEN
    MSGBOX "Improper Name. Record not accepted",,$AppTitle
    EXIT FUNCTION
END IF
IF LEN(s4)<10 OR VAL(s4)=0 THEN
    MSGBOX "Improper Mobile Number. Record not accepted",,$AppTitle
    EXIT FUNCTION
END IF

IF VAL(s5)<%TEMP_LIMIT THEN
    MSGBOX "Milk Temperature below "+STR$(%TEMP_LIMIT)+CHR$(129)+"C. Record not accepted",,$AppTitle
    EXIT FUNCTION
END IF

IF VAL(s6)<%LACTO_LIMIT THEN
    MSGBOX "Milk Density below "+STR$(%LACTO_LIMIT)+CHR$(129)+". Record not accepted",,$AppTitle
    EXIT FUNCTION
END IF

d1=VAL(s7)
d2=VAL(s8)
d3=d2-d1
IF d3<1.0 THEN
    MSGBOX "Milk Quantity low. Record not accepted",,$AppTitle
    EXIT FUNCTION
END IF
s11=USING$("*0#.##",d3)

s9=TRIM$(s4)
s10="ID="+s1+", Temp.="+s5+",Density="+s6+",Qty="+s11+" Kg. Delivered on:"+dt1.sddmmyyyy+" at "+TIME$+". Transaction ID:"+STR$(lTransactionCount+1)
SendSMS(s9,s10)

TempTransaction.sIDNumber=s1
TempTransaction.sTemperature=s5
TempTransaction.sLactoReading=s6
TempTransaction.sWeight=s11
TempTransaction.sDate=dt1.sddmmyyyy
TempTransaction.sWeight=TIME$
TempTransaction.sMobile=s4
INCR lTransactionCOunt
TransactionData(lTransactionCount)=TempTransaction
WriteTransactionFile
MSGBOX "Transaction Recorded and SMS sent to Phone number:"+s4,,$AppTitle

GdiPlusSaveImageToFileNew

END FUNCTION
'------------------------------------------------------------------------------
CALLBACK FUNCTION xExitOperateCallBack
LOCAL l1 AS LONG
l1=MSGBOX ("Confirm Exit",%MB_YESNO,$AppTitle)
IF l1=%IDYES THEN
    CameraOff(hCameraWindowHandle)
    DIALOG END CB.HNDL, 0
END IF

END FUNCTION
'================================================================================================
'SUB GDIPlus_SaveImage(hGraphicDC AS DWORD, sMimeType AS STRING, fName AS WSTRINGZ)
'================================================================================================
SUB GDIPlus_SaveImage(hGraphicDC AS DWORD, sMimeType AS STRING, fName AS WSTRINGZ)
   LOCAL s AS STRING, sEncoderClsid AS GUID, pImage, hBitmap, pGraphics AS DWORD

   s = GDIPlusGetEncoderClsid((sMimeType))
   sEncoderClsid = GUID$(s)

   hBitmap = GetCurrentObject(hGraphicDC, %OBJ_Bitmap)              'from graphic target
   GDIpCreateBitmapFromHBITMAP( hBitmap, BYVAL %Null, pImage)       'create GDI+ image (pImage)
   GdipCreateFromHDC hGraphicDC, pGraphics                          'create graphic object containing Graphic Control
'   GdipDrawImageRect pGraphics, pImage, 0, 0,  lGWW,lGWH              'use (0,0)-(140,110)
   GdipSaveImageToFile pImage,(fName), sEncoderClsid, BYVAL %Null   'save to file

   IF pImage THEN GdipDisposeImage(pImage)                          'cleanup
   IF pGraphics THEN GdipDeleteGraphics(pGraphics)   'GDIP cleanup
END SUB

'================================================================================================
'SUB GDIPlus_SaveImage(hGraphicDC AS DWORD, sMimeType AS STRING, fName AS WSTRINGZ)
'================================================================================================
SUB GDIPlus_SaveImageWithLevel(hGraphicDC AS DWORD, sMimeType AS STRING, fName AS WSTRINGZ, lLevel AS LONG)
   LOCAL s AS STRING, sEncoderClsid AS GUID, pImage, hBitmap, pGraphics AS DWORD
   LOCAL eps AS EncoderParameters
   LOCAL ep AS EncoderParameter
   LOCAL dwQuality AS DWORD

   '-----------------------Set level
   eps.count = 1
   eps.Parameter(0).pGuid = $EncoderQuality
   eps.Parameter(0).dwType = %EncoderParameterValueTypeLong
   eps.Parameter(0).NumberOfValues = 1

   ' // Save the image as a JPEG with quality level 0.
   dwQuality = lLevel
   eps.Parameter(0).Value = VARPTR(dwQuality)

   '-----------------------
   s = GDIPlusGetEncoderClsid((sMimeType))
   sEncoderClsid = GUID$(s)

   hBitmap = GetCurrentObject(hGraphicDC, %OBJ_Bitmap)              'from graphic target
   GDIpCreateBitmapFromHBITMAP( hBitmap, BYVAL %Null, pImage)       'create GDI+ image (pImage)
   GdipCreateFromHDC hGraphicDC, pGraphics                          'create graphic object containing Graphic Control
'   GdipDrawImageRect pGraphics, pImage, 0, 0, lGWW,lGWH             'use (0,0)-(140,110)
   GdipSaveImageToFile pImage,(fName), sEncoderClsid, BYVAL %Null   'save to file

   IF pImage THEN GdipDisposeImage(pImage)                          'cleanup
   IF pGraphics THEN GdipDeleteGraphics(pGraphics)   'GDIP cleanup

END SUB

'======================================================================================
' ========================================================================================
'FUNCTION GdiPlusSaveImageToFileNew () AS LONG
' creates teh required jpg file for lactometer measurement
' ========================================================================================
FUNCTION GdiPlusSaveImageToFileNew () AS LONG

LOCAL hr , hCameraDC,hBitmapDC ,hWindowDC ,x1,y1,x2,y2 AS LONG
LOCAL token AS DWORD, StartupInput AS GdiplusStartupInput
LOCAL hBitmap , pImage AS DWORD
LOCAL ws1,ws2 ,wszMimeType ,wszFileName AS WSTRINGZ * %MAX_PATH
LOCAL s AS STRING
LOCAL clsidEncoder AS GUID
wszMimeType="image/jpeg"

DIALOG UNITS hDestinationFormHandle,lGWX,lGWY TO PIXELS x1,y1 ' starting point x,y
DIALOG UNITS hDestinationFormHandle,lGWW,lGWH TO PIXELS x2,y2 ' window width and height

'MSGBOX STR$(lGWX)+STR$(lGWY)+STR$(x1)+STR$(y1)+"="+STR$(lGWW)+STR$(lGWH)+STR$(x2)+STR$(y2)

StartupInput.GdiplusVersion = 1
GdiplusStartup(token, StartupInput, BYVAL %NULL)

s = GdiPlusGetEncoderClsid(wszMimeType)
IF s = "" THEN
   FUNCTION = %StatusInvalidParameter
EXIT FUNCTION
END IF
clsidEncoder = GUID$(s)

GRAPHIC BITMAP NEW x2+10,y2+10 TO hBitmap
GRAPHIC ATTACH hBitmap,0
GRAPHIC GET DC TO hBitmapDC
CONTROL HANDLE hDestinationFormHandle,%ID_PICTURE TO hWindowDC
hCameraDC=GetDC(hCameraWindowHandle)
wszFilename=$jpgfile'"TESTZ.jpg"
BitBlt hBitmapDC, 0,0,x2*2,y2*2, hCameraDC,0,0,%SRCCopy       ' copies to bitmap in memory
GDIPlus_SaveImage(hBitmapDC, "image/jpeg", wszFileName)
wszFilename=$jpgfile1'"TESTZ1.jpg"
GDIPlus_SaveImageWithLevel(hBitmapDC, "image/jpeg", wszFileName,100)

wszFilename=$bmpfile'"TESTZ.bmp"
BitBlt hBitmapDC, 0,0,x2,y2, hCameraDC,0,0,%SRCCopy       ' copies to bitmap in memory
GDIPlus_SaveImage(hBitmapDC, "image/bmp", wszFileName)

wszFilename=$giffile'"TESTZ.gif"
BitBlt hBitmapDC, 0,0,x2,y2, hCameraDC,0,0,%SRCCopy       ' copies to bitmap in memory
GDIPlus_SaveImage(hBitmapDC, "image/gif", wszFileName)

FUNCTION = hr
GRAPHIC BITMAP END
GdiplusSHutdown token

END FUNCTION
' ========================================================================================
'------------------------------------------------------------------------------

FUNCTION EncodeParameters(lLevel AS LONG) AS LONG

   LOCAL hStatus AS LONG
   LOCAL token AS DWORD
   LOCAL StartupInput AS GdiplusStartupInput
   LOCAL EncoderClsid AS GUID
   LOCAL pImage AS DWORD
   LOCAL eps AS EncoderParameters
   LOCAL ep AS EncoderParameter
   LOCAL dwQuality AS DWORD
   LOCAL strFileName AS STRING

   ' // Initialize GDI+
   StartupInput.GdiplusVersion = 1
   hStatus = GdiplusStartup(token, StartupInput, BYVAL %NULL)
   IF hStatus THEN
      FUNCTION=0'PRINT "Error initializing GDI+"
      EXIT FUNCTION
   END IF

'   strFileName = UCODE$("Shapes.bmp")
'   hStatus = GdipLoadImageFromFile(STRPTR(strFileName), pImage)

   ' // Get the CLSID of the JPEG encoder.
'   EncoderClsid = GUID$(GdiPlusGetEncoderClsid("image/jpeg"))

   ' // Before we call GdipSaveImageToFile, we must initialize an
   ' // EncoderParameters structure The EncoderParameters structure
   ' // has an array of EncoderParameter structures In this
   ' // case, there is only one EncoderParameter structure in the array.
   ' // The one EncoderParameter structure has an array of values.
   ' // In this case, there is only one value (of type DWORD)
   ' // in the array. We will let this value vary from 0 to 100.

   eps.count = 1
   eps.Parameter(0).pGuid = $EncoderQuality
   eps.Parameter(0).dwType = %EncoderParameterValueTypeLong
   eps.Parameter(0).NumberOfValues = 1

   ' // Save the image as a JPEG with quality level 0.
   dwQuality = 0
   eps.Parameter(0).Value = VARPTR(dwQuality)
   strFileName = UCODE$("Shapes001.jpg")
   hStatus = GdipSaveImageToFile(pImage, (strFileName), EncoderClsid, eps)
   IF hStatus = %StatusOk THEN
       'PRINT "Shapes001.jpg saved successfully"
   END IF

   ' // Save the image as a JPEG with quality level 50.
   dwQuality = 50
   eps.Parameter(0).Value = VARPTR(dwQuality)
   strFileName = UCODE$("Shapes050.jpg")
   hStatus = GdipSaveImageToFile(pImage, (strFileName), EncoderClsid, eps)
   IF hStatus = %StatusOk THEN
       'PRINT "Shapes050.jpg saved successfully"
   END IF

   ' // Save the image as a JPEG with quality level 100.
   dwQuality = 100
   eps.Parameter(0).Value = VARPTR(dwQuality)
   strFileName = UCODE$("Shapes100.jpg")
   hStatus = GdipSaveImageToFile(pImage, (strFileName), EncoderClsid, eps)
   IF hStatus = %StatusOk THEN
       'PRINT "Shapes100.jpg saved successfully"
   END IF

   ' // Cleanup
   IF pImage THEN GdipDisposeImage(pImage)

   ' // Shutdown GDI+
   GdiplusShutdown token


END FUNCTION
'-------------------------------------------------------------------------------

FUNCTION ACIR(sResult AS STRING) AS STRING
LOCAL lfile AS LONG
LOCAL s1,s2 AS STRING
lfile=FREEFILE
s1=""
ON ERROR GOTO ACIR_error
OPEN $TestFile FOR INPUT AS #lfile
INPUT #lfile,s1
IF LEN(s1)=0 THEN
    s1=sResult
ELSE
    s2=sResult
END IF
CLOSE #lfile

FUNCTION=TRIM$(s1)
EXIT FUNCTION
ACIR_error:
FUNCTION=sResult
END FUNCTION
'--------------------------------------------------------------------------------

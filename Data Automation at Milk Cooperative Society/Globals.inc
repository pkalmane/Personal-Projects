'Globals.inc
$AppTitle="Milk Data Automation Ver 1.3"
$EXEFile="opencv_DIP.exe"
$EXEFilestd="opencv demo.exe"
$InterProcessFile="C:\ProgramData\Temp\milkcmd.txt"
$InterProcessResult="C:\ProgramData\Temp\milkres.txt"
$InterProcessExitFile="C:\ProgramData\Temp\milkexit.txt"
$INIFile="C:\ProgramData\Temp\Lactometer.ini"
$MasterFile="C:\ProgramData\Temp\Master.dat"
$MasterFileBackup1="C:\ProgramData\Temp\Master1.dat"
$MasterFileBackup2="C:\ProgramData\Temp\Master2.dat"
$TransactionFile="C:\ProgramData\Temp\Transaction.dat"
$TransactionFileBackup1="C:\ProgramData\Temp\Transaction1.dat"
$TransactionFileBackup2="C:\ProgramData\Temp\Transaction2.dat"
$jpgFile="C:\ProgramData\Temp\MilkData.jpg"
$jpgFile1="C:\ProgramData\Temp\MilkData1.jpg"
$bmpFile="C:\ProgramData\Temp\MilkData.bmp"
$gifFile="C:\ProgramData\Temp\MilkData.gif"
$MasterReport="C:\ProgramData\Temp\MasterReport.txt"
$TransactionReport="C:\ProgramData\Temp\TransactionReport.txt"
$WeightToVolume="1.25"
$TestFile="C:\ProgramData\Temp\m.txt"

$CTLZ=CHR$(&H1A)
%TEMP_LIMIT=15      ' if milk temperature is below this limit, it will be rejected
%LACTO_LIMIT=1
GLOBAL hMainDialogHandle,hDlg1,hFont1 AS LONG
GLOBAL ghDC AS DWORD ' handle to graphic object
GLOBAL lLinkFLag AS LONG
GLOBAL lNameError AS LONG  ' flag used in Newdata funciton. When set, indicates there is 

an error in firstname

TYPE TypPriceData
    sFatValue AS STRING *5
    sPrice AS STRING *5
END TYPE

TYPE TypFarmerData
    sIDnumber AS STRING*16
    sDOR        AS STRING * 10
    sLastName  AS STRING * 32
    sFirstName AS STRING * 32
    sAddress1  AS STRING * 64
    sAddress2  AS STRING * 64
    sAddress3  AS STRING * 64
    sTaluk     AS STRING * 24
    sDistrict  AS STRING * 4
    sPincode   AS STRING * 12
    sCattleCount AS STRING*4
    sCattleType AS STRING *4
    sLandline  AS STRING * 12
    sMobile    AS STRING * 12
    sAadhar    AS STRING * 16
    sEmail     AS STRING * 64
    sComments  AS STRING * 128
END TYPE

TYPE TypTransactionData
    sIDnumber AS STRING*16
    sTemperature  AS STRING * 4
    sLactoReading  AS STRING * 4
    sWeight AS STRING *8
    sDate AS STRING * 8
    sTime AS STRING * 8
    sMobile AS STRING * 12
END TYPE


GLOBAL PriceData() AS TypPriceData    ' contains price table
GLOBAL lPriceCount AS LONG
GLOBAL FarmerData() AS TypFarmerData
GLOBAL lFarmerCOunt AS LONG
GLOBAL TransactionData() AS TypTransactionData
GLOBAL lTransactionCount AS LONG

GLOBAL lComportFileRFID,lComportFileModem,hThread1 AS LONG
GLOBAL lDestinationTextBox AS LONG ' contains ID number of the text box where RFID reader 

should ut data
GLOBAL hDestinationFormHandle AS LONG ' contains handle to the form which ahs textbox of 

interest
GLOBAL lComportRFID, lComportModem AS LONG
GLOBAL sModemReceiveData AS STRING      ' contains data receibed from modem com port
GLOBAL gCurrentEntry , gEditButton ,gOperationFlag AS LONG
GLOBAL hCameraWindowHandle,hPictureHandle AS LONG
GLOBAL wsZText AS WSTRINGZ *128
GLOBAL gsRFID AS STRING
GLOBAL lGWX,lGWY,lGWW,lGWH AS LONG
GLOBAL sModemCommunication AS STRING
'------------------------------------------------------------------------------
' Global data variables
'------------------------------------------------------------------------------
GLOBAL gIsRecModified  AS LONG
GLOBAL gIsFileModified AS LONG
GLOBAL gCurrentEntry   AS LONG
GLOBAL gMaxEntries     AS LONG
GLOBAL gsFindText      AS STRING



%IDC_BTNEDITDATA = 1002
%IDC_BTNEXIT     = 1004
%IDC_BTNNEWDATA  = 1001
%IDC_BTNREPORTS  = 1003
%IDC_LABEL1      = 1005
%IDD_DIALOG1     =  101
%IDC_TEXTBOX_COMPORT =1100
%IDC_LABEL10 = 1110
%IDC_TEXTBOX_RFID=1120
%IDC_BTNOPERATE=1130
%IDC_BTNSETTINGS    =1140
%ID_MILK_TEMPERATURE_TEXTBOX=1240
%ID_LACTO_READING_TEXTBOX=1250
%ID_PRE_WEIGHT_TEXTBOX=1260
%ID_POST_WEIGHT_TEXTBOX=1270

%ID_NEW_MODEM_TEXTBOX=1160

%IDC_STATUSBAR=1170
%ID_PICTURE = 1180
%ID_INDEX       = 100

' Edit Control IDs
%ID_LASTNAME    = 102
%ID_FIRSTNAME   = 103
%ID_ADDRESS1    = 104
%ID_ADDRESS2    = 105
%ID_ADDRESS3    = 106
%ID_TALUK       = 107
%ID_DISTRICT    = 108
%ID_PINCODE     = 109
%ID_OPTION1     = 110
%ID_OPTION3     = 121
%ID_LANDLINE    = 111
%ID_EMAIL       = 113
%ID_COMMENTS    = 115
%ID_CATNUM      = 120
%ID_CATTYPE     =002
%ID_MOBILE      =003
%ID_AADHAR       =004
%id_NEW_RFID_TEXTBOX =005
%iD_DOR     =006
%ID_LISTBOX1        = 121
%ID_CHECKBOX1      =122
%ID_CHECKBOX2      =123
%ID_CHECKBOX3      =124

' Button control IDs
%ID_PRIOR       = 121
%ID_NEXT        = 122
%ID_ADD         = 123
%ID_UPDATE      = 124
%ID_FIND        = 125
%ID_DELETE      = 126
%ID_PRINT       = 127

' Menu control IDs
%ID_MENU_CLOSE  = %IDCANCEL
%ID_MENU_CUT    = 201
%ID_MENU_COPY   = 202
%ID_MENU_PASTE  = 203
%ID_MENU_HELP   = 204
%ID_MENU_ABOUT  = 205


'  %ID_NEW_RFID_TEXTBOX=1200
'  %ID_FIRSTNAME=1210
'  %ID_LASTNAME=1220
'  %ID_MOBILE=1230
  %ID_CLEAR =1280
  %ID_WEIGHT =1290
%IDC_CALENDAR=1300
%ID_MODEM_TEXTBOX=1310

' Graphic windows size at x,y width of W and height of H
%GWX=210
%GWY=10
%GWW=380
%GWH=280

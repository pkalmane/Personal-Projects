
'------------------------------------------------------------------------------
' user interface
'------------------------------------------------------------------------------
%USEMACROS = 1
#INCLUDE ONCE "Globals.inc"
#INCLUDE ONCE "clsDate.inc"
#INCLUDE ONCE "cWindow.inc"

'------------------------------------------------------------------------------
' Equates
'------------------------------------------------------------------------------
'$AppTitle = "Address Book - PBWin DDT Example"

' Edit Control IDs

'------------------------------------------------------------------------------
FUNCTION ReportFunction() AS LONG
    gCurrentEntry=1
    ShowReportDIALOG()
END FUNCTION
'-----------------------------------------------------------------------------


'------------------------------------------------------------------------------
' Data structures
'------------------------------------------------------------------------------



'------------------------------------------------------------------------------
' Callback Declarations
'------------------------------------------------------------------------------
DECLARE CALLBACK FUNCTION xAddressCallBack()
DECLARE CALLBACK FUNCTION xPriorNextCallBack()
DECLARE CALLBACK FUNCTION xAddUpdateCallBack()
DECLARE CALLBACK FUNCTION xDeleteCallBack()
DECLARE CALLBACK FUNCTION xOkCallBack()
DECLARE CALLBACK FUNCTION xFindCallBack()
DECLARE CALLBACK FUNCTION xFindDialogCallBack()
DECLARE CALLBACK FUNCTION xCancelCallBack()
DECLARE CALLBACK FUNCTION xPrintCallBack()


'------------------------------------------------------------------------------
' Helper Sub/Function Declarations
'------------------------------------------------------------------------------
DECLARE SUB DialogToUdt(BYVAL hDlg AS DWORD, Index AS LONG)
DECLARE SUB UdtToDialog(BYVAL hDlg AS DWORD, Index AS LONG)
DECLARE SUB xSetButtonStatus(BYVAL hDlg AS DWORD)
DECLARE SUB AddCountries(BYVAL hDlg AS DWORD)


'------------------------------------------------------------------------------
' Main entry point for the application
'------------------------------------------------------------------------------
FUNCTION ShowReportDIALOG () AS LONG

    REGISTER x       AS LONG
    LOCAL    hDlg    AS DWORD
    LOCAL    hMenu   AS DWORD
    LOCAL    Result  AS LONG
    LOCAL    hPopup1 AS DWORD
    LOCAL    hPopup2 AS DWORD
    LOCAL    nFile   AS LONG

    gMaxEntries=lFarmerCount
    ' Create our primary GUI
    DIALOG NEW 0, $AppTitle+" - Reports", 40,40,280,40, _
        %DS_CENTER OR %WS_CAPTION OR %WS_SYSMENU, 0 TO hDlg

    ' Abort if the dialog could not be created
    IF hDlg = 0 THEN EXIT FUNCTION  ' Error occurred

    DIALOG SET COLOR hDlg, -1&, RGB(255,255,222)
    CONTROL ADD BUTTON, hDlg,19,"&Master Report",10,10,60,20, CALL btnMasterCallback
    CONTROL ADD BUTTON, hDlg,20,"&Transaction Report",80,10,70,20, CALL btnTransactionCallback
    CONTROL ADD BUTTON, hDlg,20,"&Price Report",160,10,70,20, CALL btnPriceCallback

'    CONTROL ADD "SysMonthCal32", hDlg, %IDC_CALENDAR, "",     0, 26, 396, 190, _
'        %WS_CHILD OR %WS_VISIBLE OR %WS_TABSTOP OR %MCS_MULTISELECT, _
'        %WS_EX_CLIENTEDGE
'
'    ' Enable selection of up to 31 days.
'    ' See COMMCTRL.INC for other useful messages, equates and Type structures
'    CONTROL SEND hDlg, %IDC_CALENDAR, %MCM_SETMAXSELCOUNT, 31, 0

    CONTROL ADD BUTTON, hDlg,21,"E&xit",240,10,20,20, CALL btnReportExitCallback

    ' Color the LABEL controls (foreground = default, background = transparent)
    FOR x = 19 TO 21
        CONTROL SET COLOR hDlg, x, -1, -2
    NEXT x

    lDestinationTextBox=%id_NEW_RFID_TEXTBOX
    hDestinationFormHandle=hDlg

    ' Start up our GUI, and run until the user quits
    DIALOG SHOW MODAL hDlg  TO Result


END FUNCTION

'------------------------------------------------------------------------------
'function CenterString(sData as string, sleft as string, sright as string) as string
'------------------------------------------------------------------------------
FUNCTION CenterString(sData AS STRING, sleft AS STRING, sright AS STRING) AS STRING
LOCAL s1,s2,s3 AS STRING
LOCAL l1 AS LONG
s1=sData
l1=LEN(s1)
DO WHILE (l1<80)
    s2=WRAP$(s1,sleft,sright)
    s1=s2
    l1=LEN(s1)
LOOP
FUNCTION=s1
END FUNCTION
'------------------------------------------------------------------------------
'CALLBACK FUNCTION btnMasterCallback
'------------------------------------------------------------------------------
CALLBACK FUNCTION btnMasterCallback
LOCAL lSlno,l1,l2,l3, lFile  AS LONG
LOCAL sReport AS STRING*80
LOCAL s1,s2 , sline AS STRING
sline=STRING$(80,"-")
MSGBOX "Generating Master List"
lFIle=FREEFILE

OPEN $MasterReport FOR OUTPUT AS #lFile
PRINT #lFile,sline
s1=CenterString("MASTER REPORT"," "," ")
PRINT#lFile,s1
PRINT #lFile,sline
PRINT #lFile,"Slno";TAB(8);"ID NUmber";TAB(20);"Name and Address";TAB(54);"Mobile Number";TAB(70);"Date of Reg"
PRINT #lFile,sline

lSlno=0
FOR l1=1 TO lFarmerCount
    INCR lSlno

    PRINT #lFile,USING$("* #",lSlNo);
    PRINT #lFile,TAB(8);TRIM$(FarmerData(l1).sIDNUmber);
    PRINT #lFile,TAB(20);TRIM$(FarmerData(l1).sFirstName)+" "+TRIM$(FarmerData(l1).sLastName)
    PRINT #lFile,TAB(20);TRIM$(FarmerData(l1).sAddress1);TAB(54);TRIM$(FarmerData(l1).sLandline);TAB(70);TRIM$(FarmerData(l1).sDOR)
    PRINT #lFile,TAB(20);TRIM$(FarmerData(l1).sAddress2);TAB(54);TRIM$(FarmerData(l1).sMobile)
    PRINT #lFile,TAB(20);TRIM$(FarmerData(l1).sAddress3)
    PRINT #lFile,TAB(20);TRIM$(FarmerData(l1).sTaluk)+" "+TRIM$(FarmerData(l1).sDistrict)
    PRINT #lFile,TAB(20);"Pin - "+FarmerData(l1).sPinCode
    PRINT #lFile,sline

'TYPE TypFarmerData
'    sIDnumber AS STRING*16
'    sDOR        AS STRING * 10
'    sLastName  AS STRING * 32
'    sFirstName AS STRING * 32
'    sAddress1  AS STRING * 64
'    sAddress2  AS STRING * 64
'    sAddress3  AS STRING * 64
'    sTaluk     AS STRING * 24
'    sDistrict  AS STRING * 4
'    sPincode   AS STRING * 12
'    sCattleCount AS STRING*4
'    sCattleType AS STRING *4
'    sLandline  AS STRING * 12
'    sMobile    AS STRING * 12
'    sAadhar    AS STRING * 16
'    sEmail     AS STRING * 64
'    sComments  AS STRING * 128
'END TYPE


NEXT l1
CLOSE #lFile

MSGBOX "File "+$MasterReport+" Generated. Opening in NOTEPAD"
l1=SHELL("notepad.exe "+$MasterReport)

END FUNCTION

'------------------------------------------------------------------------------
'CALLBACK FUNCTION btnTransactionCallback
'------------------------------------------------------------------------------
CALLBACK FUNCTION btnTransactionCallback

LOCAL lSlno,l1,l2,l3, lFile  AS LONG
LOCAL sReport AS STRING*80
LOCAL s1,s2 , sline AS STRING
sline=STRING$(80,"-")
MSGBOX "Generating Transaction Report"
lFIle=FREEFILE

OPEN $TransactionReport FOR OUTPUT AS #lFile
PRINT #lFile,sline
s1=CenterString("TRANSACTION REPORT"," "," ")
PRINT#lFile,s1
PRINT #lFile,sline
PRINT #lFile,"Slno";TAB(8);"ID NUmber";TAB(20);"Temp.";TAB(36);"Lacto.Reading";TAB(52);"Weight   ";TAB(68);"Date Time"
PRINT #lFile,sline

FOR l1=1 TO lTransactionCount
    INCR lSlno

    PRINT #lFile,USING$("* #",lSlNo);
    PRINT #lFile,TAB(8);TRIM$(TransactionData(l1).sIDNUmber);
    PRINT #lFile,TAB(20);TRIM$(TransactionData(l1).sTemperature);
    PRINT #lFile,TAB(36);TRIM$(TransactionData(l1).sLactoReading);
    PRINT #lFile,TAB(52);TRIM$(TransactionData(l1).sWeight)+" Kg";
    PRINT #lFile,TAB(68);TRIM$(TransactionData(l1).sDate)
    PRINT #lFile,TAB(68);TRIM$(TransactionData(l1).sTime)

'TYPE TypTransactionData
'    sIDnumber AS STRING*16
'    sTemperature  AS STRING * 4
'    sLactoReading  AS STRING * 4
'    sWeight AS STRING *8
'    sDate AS STRING * 8
'    sTime AS STRING * 8
'END TYPE

NEXT l1
CLOSE #lFile

MSGBOX "File "+$TransactionReport +" Generated. Opening in NOTEPAD"
l1=SHELL("notepad.exe "+$TransactionReport)

END FUNCTION
'------------------------------------------------------------------------------
'CALLBACK FUNCTION btnPriceCallback
'------------------------------------------------------------------------------
CALLBACK FUNCTION btnPriceCallback

LOCAL lSlno,l1,l2,l3, lFile  AS LONG
LOCAL sReport AS STRING*80
LOCAL s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,sDate,sFatValue,sline , sAmount AS STRING

LOCAL d1 AS DateInterface
LET d1=CLASS "clsDate"
s1=d1.sddmmyyyy

sline=STRING$(80,"-")
MSGBOX "Calculating Amounts to be paid"
sDate=CWindow_InputBox (hDestinationFormHandle,20,20,"Date for which amount is to be calculated","Enter date as DD/MM/YYYY",s1)
sFatValue=CWindow_InputBox (hDestinationFormHandle,20,20,"Fat Content for the day","Enter Fat value (example:5.2)","3.5")

'FUNCTION CWindow_InputBox (BYVAL hParent AS DWORD, BYVAL x ASstrPrompt AS STRING, BYVAL strText AS STRING, OPTIONAL BYVAL nLen AS LONG, BYVAL bPassword AS LONG) AS STRING

lFIle=FREEFILE

OPEN $TransactionReport FOR OUTPUT AS #lFile
PRINT #lFile,sline
s1=CenterString("TRANSACTION REPORT"," "," ")
PRINT#lFile,s1, sDate,sFatValue
PRINT #lFile,sline
PRINT #lFile,"Slno";TAB(8);"ID NUmber";TAB(20);"Temp.";TAB(36);"Lacto.Reading";TAB(52);"Weight   ";TAB(68);"Date Time"
PRINT #lFile,sline

FOR l1=1 TO lTransactionCount
    s1=TransactionData(l1).sDate
    IF MID$(sDate,1,2)=MID$(s1,1,2) AND MID$(sDate,4,2)=MID$(s1,4,2) AND MID$(sDate,7,4)=MID$(s1,7,4) THEN
        INCR lSlno

        PRINT #lFile,USING$("* #",lSlNo);
        s1=TRIM$(TransactionData(l1).sIDNUmber):PRINT #lFile,TAB(8);s1
        s2=TRIM$(TransactionData(l1).sTemperature):PRINT #lFile,TAB(20);s2;
        s3=TRIM$(TransactionData(l1).sLactoReading):PRINT #lFile,TAB(36);s3;
        s4=TRIM$(TransactionData(l1).sWeight)+" Kg ":s5=USING$("##.#",(VAL(s4)*VAL($WeightToVolume))):s5=s5+" Litres ":PRINT #lFile,TAB(52);s4+s5;
        s6=TRIM$(TransactionData(l1).sDate):PRINT #lFile,TAB(68);s6
        s7=TransactionData(l1).sTime:PRINT #lFile,TAB(68);s7

        sAmount="0.0"
        FOR l2=1 TO lPriceCOunt
            IF TRIM$(PriceData(l2).sFatValue)=TRIM$(sFatValue) THEN
                sAmount=USING$("Rs ###.##",VAL(s5)*VAL(TRIM$(PriceData(l2).sPrice)))
                EXIT FOR
            END IF
        NEXT l2


        s10="ID="+s1+",Delivery date ="+s6+" "+s7+",Density="+s3+",Qty="+s4+s5+" Temp.="+s2+". Amount="+sAmount
        s9=TransactionData(l1).sMobile
        SendSMS(s9,s10)

    END IF
NEXT l1
CLOSE #lFile

MSGBOX "File "+$TransactionReport +" Generated. Opening in NOTEPAD"
l1=SHELL("notepad.exe "+$TransactionReport)
END FUNCTION
'------------------------------------------------------------------------------
'CALLBACK FUNCTION btnExitCallback
'------------------------------------------------------------------------------
CALLBACK FUNCTION btnReportExitCallback

    DIALOG END hDestinationFormHandle

END FUNCTION
'------------------------------------------------------------------------------
' Main dialog callback
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
' user interface
'------------------------------------------------------------------------------
%USEMACROS = 1
#INCLUDE ONCE "Globals.inc"

'------------------------------------------------------------------------------
' Function dclares
'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
FUNCTION SettingsFunction() AS LONG
    ShowSettingsDIALOG()
END FUNCTION
'-----------------------------------------------------------------------------

'------------------------------------------------------------------------------
' Main entry point for the application
'------------------------------------------------------------------------------
FUNCTION ShowSettingsDIALOG () AS LONG

    REGISTER x       AS LONG
    LOCAL    hDlg    AS DWORD
    LOCAL    hMenu   AS DWORD
    LOCAL    Result  AS LONG
    LOCAL    hPopup1 AS DWORD
    LOCAL    hPopup2 AS DWORD
    LOCAL    nFile   AS LONG

    DIALOG FONT "Tahoma"

    ' Create our primary GUI
    DIALOG NEW 0, $AppTitle + " - Settings", 77, 22, 140, 80, _
        %DS_CENTER OR %WS_CAPTION OR %WS_SYSMENU, 0 TO hDlg

    ' Abort if the dialog could not be created
    IF hDlg = 0 THEN EXIT FUNCTION  ' Error occurred

    DIALOG SET COLOR hDlg, -1&, RGB(255,255,222)

    ' Now we create our controls.  We create the static control immediately before the
    ' real control so that the keyboard accelerators switch focus to the real control.


    CONTROL ADD LABEL,    hDlg, 20,  "Comport for RFID Reader", 10,  20,  100,  10
    CONTROL ADD TEXTBOX,    hDlg, %ID_NEW_RFID_TEXTBOX, "1", 110,   20,  20,  10,%SS_CENTER

    CONTROL ADD LABEL,    hDlg, 21,  "Comport for GSM Modem", 10,  40,  100,  10
    CONTROL ADD TEXTBOX,    hDlg, %ID_NEW_MODEM_TEXTBOX, "2", 110,   40,  20,  10,%SS_CENTER

    ' Add the command buttons
    CONTROL ADD BUTTON,   hDlg, %ID_PRIOR, "SET",  10, 60,  40,  14 CALL xSetCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_WEIGHT, "CANCEL",  55, 60,  40,  14 CALL xCancelSettingsCallBack

    FOR x = 19 TO 34
        CONTROL SET COLOR hDlg, x, -1, -2
    NEXT x

    lDestinationTextBox=%id_NEW_RFID_TEXTBOX
    hDestinationFormHandle=hDlg

    ' Start up our GUI, and run until the user quits
    DIALOG SHOW MODAL hDlg TO Result

    '----------------------------------------------------------------

END FUNCTION


'------------------------------------------------------------------------------
CALLBACK FUNCTION xSetCallBack
LOCAL s1 AS STRING
LOCAL l1 AS LONG
    CONTROL GET TEXT hDestinationFormHandle,%ID_NEW_RFID_TEXTBOX TO s1
    s1=TrimRFID(s1)
    l1=VAL(TRIM$(s1))
    IF l1 >0 THEN
        lComportRFID=l1
    END IF

    CONTROL GET TEXT hDestinationFormHandle,%ID_NEW_MODEM_TEXTBOX TO s1
    l1=VAL(TRIM$(s1))
    IF l1 >0 THEN
        lComportModem=l1
    END IF

    PrepareCOmport()

    DIALOG END CB.HNDL, 0

END FUNCTION

'------------------------------------------------------------------------------
CALLBACK FUNCTION xCancelSettingsCallBack
    DIALOG END CB.HNDL, 0
END FUNCTION

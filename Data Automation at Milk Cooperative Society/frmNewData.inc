
'------------------------------------------------------------------------------
' user interface
'------------------------------------------------------------------------------
%USEMACROS = 1
#INCLUDE ONCE "Globals.inc"
#INCLUDE ONCE "clsDate.inc"

'------------------------------------------------------------------------------
' Equates
'------------------------------------------------------------------------------
'$AppTitle = "Address Book - PBWin DDT Example"

' Edit Control IDs

'------------------------------------------------------------------------------
FUNCTION NewDataFunction() AS LONG
    gCurrentEntry=1
    ShowNewDataDIALOG()
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
FUNCTION ShowNewDataDIALOG () AS LONG

    REGISTER x       AS LONG
    LOCAL    hDlg    AS DWORD
    LOCAL    hMenu   AS DWORD
    LOCAL    Result  AS LONG
    LOCAL    hPopup1 AS DWORD
    LOCAL    hPopup2 AS DWORD
    LOCAL    nFile   AS LONG

    gMaxEntries=lFarmerCount
    ' Create our primary GUI
    DIALOG NEW 0, $AppTitle+" - New Data", 77, 22, 400, 267, _
        %DS_CENTER OR %WS_CAPTION OR %WS_SYSMENU, 0 TO hDlg

    ' Abort if the dialog could not be created
    IF hDlg = 0 THEN EXIT FUNCTION  ' Error occurred

    DIALOG SET COLOR hDlg, -1&, RGB(255,255,222)

    ' Now we create our controls.  We create the static control immediately before the
    ' real control so that the keyboard accelerators switch focus to the real control.

    CONTROL ADD LABEL,    hDlg, 19,  "DATE OF REG.", 5,   5,  60,  12 ,%SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_DOR,    "31/03/2014",  68, 4, 60, 12

    CONTROL ADD LABEL,    hDlg, 20,  "*USER ID.NO", 240,   4,  80,  10,%SS_CENTER
    CONTROL ADD TEXTBOX,    hDlg, %ID_NEW_RFID_TEXTBOX, "1234567890", 300,   4,  80,  10',,    ,call RFIDChangeCallback


    CONTROL ADD LABEL,    hDlg, 21,       "*First Name",   5,  33,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_FirstName,    "102",  68,  31, 90,  12,,,CALL NameCallback

    CONTROL ADD LABEL,    hDlg, 23,           "Last Name", 245,  33,  50,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_LastName,   "103", 300,  31, 90,  12

    CONTROL ADD LABEL,    hDlg, 24,         "*Address",   5,  48,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_Address1,    "104",  68,  46, 323,  12
    CONTROL ADD TEXTBOX,  hDlg, %id_Address2,    "105",  68,  61, 323,  12
    CONTROL ADD TEXTBOX,  hDlg, %id_Address3,    "106",  68,  76, 323,  12

    CONTROL ADD LABEL,    hDlg, 25,            "*Pin Code",   5,  93,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_PINCODE,    "560001",  68,  91, 116,  12 ,,,CALL PincodeCallback

    CONTROL ADD LABEL,    hDlg, 26,  "Place", 158,  93,  50,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_taluk,   " ", 211,  91,  100,  12

    CONTROL ADD LABEL,    hDlg, 27, "State",5, 109,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_DISTRICT,     " ",  68, 107,  40,  12

    CONTROL ADD LABEL,    hDlg, 28,         "Cattle.Qty", 110, 109,  50,8,%SS_RIGHT
    CONTROL ADD TEXTBOX,  hdlg, %id_catnum,      "5",  160, 107, 50,10

    CONTROL ADD LABEL,    hDlg, 29,         "Type of Cattle", 200, 109,  65,8,%SS_RIGHT
    CONTROL ADD TEXTBOX,  hdlg, %id_cattype,      "xyz",  270, 107, 50,10


    CONTROL ADD LABEL,    hDlg, 30,           "Land Line No.",   5, 127,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_LANDLINE,       "111",  68, 125, 100,  12

    CONTROL ADD LABEL,    hDlg, 31,           "*Mobile.no",   160, 127,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_Mobile,       "111",  228, 125, 100,  12

    CONTROL ADD LABEL,    hDlg, 32,           "Aadhar Card.no",   5, 144,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %ID_AADHAR,       "xyz",  68, 142, 100,  12

    CONTROL ADD LABEL,    hDlg, 33,           "Email",   160, 144,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_Email,       "xyz",  228, 142, 100,  12



    CONTROL ADD LABEL,    hDlg, 34,        "C&omments",   5, 160,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_Comments,    "115",  68, 158, 323,  65, %ES_AUTOHSCROLL OR %ES_AUTOVSCROLL OR %ES_MULTILINE OR %ES_WANTRETURN OR %WS_TABSTOP OR %WS_VSCROLL, %WS_EX_CLIENTEDGE

    ' Color the LABEL controls (foreground = default, background = transparent)
    FOR x = 19 TO 34
        CONTROL SET COLOR hDlg, x, -1, -2
    NEXT x


    ' Add the command buttons
    CONTROL ADD BUTTON,   hDlg, %ID_PRIOR, "<<  &Prior",  10, 233,  40,  14 CALL xPriorNextCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_NEXT,   "&Next  >>",  55, 233,  40,  14 CALL xPriorNextCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_FIND,      "&Find",  105, 233,  40,  14 CALL xFindCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_ADD,        "&Add",  155, 233,  40,  14 CALL xAddUpdateCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_UPDATE,  "U&pdate",  200, 233,  40,  14 CALL xAddUpdateCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_DELETE,  "&Delete",  245, 233,  40,  14 CALL xDeleteCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_PRINT,    "&Print",  295, 233,  40,  14 CALL xPrintCallBack
    CONTROL ADD BUTTON,   hDlg, %IDCANCEL,    "&Close",  350, 233,  40,  14 CALL xCancelCallBack

    ' Add a menu bar to the GUI
    MENU NEW BAR TO hMenu

    ' Create a popup menu
    MENU NEW POPUP TO hPopup1
    MENU ADD STRING, hPopup1, "&Close", %IDCANCEL, %MF_ENABLED
    ' Attach the popup menu to the menu bar
    MENU ADD POPUP,  hMenu,   "&File",  hPopup1,   %MF_ENABLED

    ' Create a popup menu
    MENU NEW POPUP TO hPopup2
    MENU ADD STRING, hPopup2, "&Help",  %id_Menu_Help,  %MF_ENABLED
    MENU ADD STRING, hPopup2, "-",      0,              0
    MENU ADD STRING, hPopup2, "&About", %id_Menu_About, %MF_ENABLED
    MENU ADD POPUP,  hMenu,   "&Help",  hPopup2,        %MF_ENABLED

    ' Attach the popup menu to the menu bar
    MENU ATTACH hMenu, hDlg

    ' Initialize our combobox Country List

    ' Update our "scroll" buttons
    xSetButtonStatus hDlg

    ' Set the 1st address from the UDT array into the GUI
    xUdtToDialog hDlg, gCurrentEntry


    lDestinationTextBox=%id_NEW_RFID_TEXTBOX
    hDestinationFormHandle=hDlg

    ' Start up our GUI, and run until the user quits
    DIALOG SHOW MODAL hDlg, CALL xAddressCallBack TO Result

    '----------------------------------------------------------------
    ' Save our data file if any changes were made
    IF gIsFileModified THEN
        WriteMasterFile
    END IF
    '----------------------------------------------------------------

END FUNCTION


'------------------------------------------------------------------------------
' Main dialog callback
'------------------------------------------------------------------------------
CALLBACK FUNCTION xAddressCallBack

    LOCAL Dummy AS LONG
    LOCAL hDlg  AS DWORD


    LOCAL d1 AS DateInterface
    LOCAL s1 AS STRING

    LET d1=CLASS "clsDate"
    s1=d1.sddmmyyyy

    SELECT CASE CB.MSG

    CASE %WM_INITDIALOG
            CONTROL SET TEXT CB.HNDL,%id_DOR,s1
            xSetButtonStatus CBHNDL

    CASE %WM_COMMAND
        ' If any control is edited, we need to register this so the ADD and
        ' UPDATE buttons can be enabled.
        IF CB.CTLMSG = %EN_UPDATE THEN       ' *Any* edit control was changed
            ' If this is the first "change" message, we need to update the
            ' controls in the dialog box.
            IF ISFALSE gIsRecModified THEN
                gIsRecModified = %TRUE
                xSetButtonStatus CBHNDL
            END IF
        END IF

        ' Now process any other %WM_COMMAND message from controls that don't
        ' have their own callback functions.
        SELECT CASE CB.CTL

        CASE %id_Menu_Help
            Dummy = SHELL("WINHELP.EXE", 1)

        CASE %id_Menu_About
            ' About dialog
            DIALOG NEW CB.HNDL, "New Data", 0, 0, 170, 60, _
                        %DS_MODALFRAME OR %WS_CAPTION OR %WS_SYSMENU OR %DS_CENTER TO hDlg
            DIALOG SET COLOR hDlg, -1&, %BLUE
            IF hDlg = 0 THEN EXIT FUNCTION  ' Error occurred
            CONTROL ADD LABEL, hDlg, -1, $AppTitle , _
                        15, 10, 130, 34, %SS_CENTER
            CONTROL SET COLOR hDlg, -1, %WHITE, %BLUE
            CONTROL ADD BUTTON, hDlg, 1, "&Ok", 60, 42, 40, 14 CALL xOkCallBack
            BEEP
            DIALOG SHOW MODAL hDlg
        END SELECT

    END SELECT

END FUNCTION


'------------------------------------------------------------------------------
' RFIDChangeCallback
'------------------------------------------------------------------------------
CALLBACK FUNCTION RFIDChangeCallback
LOCAL s1 AS STRING

    CONTROL GET TEXT hDestinationFormHandle,lDestinationTextBox TO s1

END FUNCTION

'------------------------------------------------------------------------------
' PRIOR/NEXT Button callback function
'------------------------------------------------------------------------------
CALLBACK FUNCTION xPriorNextCallBack

    ' Determine which control is originating the message
    IF CB.CTL = %id_Prior THEN       ' "Prior" control
        gCurrentEntry = MAX&(gCurrentEntry - 1, 1)
    ELSE                            ' "Next" control
        gCurrentEntry = MIN&(gCurrentEntry + 1, gMaxEntries)
    END IF

    ' Disregard any existing editing if one of these button clicked
    gIsRecModified = %FALSE

    ' Update the dialog
    xUdtToDialog CB.HNDL, gCurrentEntry

    ' Update the Prior/Next control button status
    xSetButtonStatus CB.HNDL

END FUNCTION
'------------------------------------------------------------------------------
' ADD and UPDATE Buttons callback function
'------------------------------------------------------------------------------



'------------------------------------------------------------------------------

'------------------------------------------------------------------------------
' ADD and UPDATE Buttons callback function
'------------------------------------------------------------------------------
CALLBACK FUNCTION xAddUpdateCallBack

    ' This callback only runs if gIsRecModified is %TRUE
    LOCAL hDlg      AS DWORD
    LOCAL s1  AS STRING

    ' User ID  not specified
    CONTROL GET TEXT CB.HNDL, %ID_NEW_RFID_TEXTBOX TO s1
    s1=trimRFID(s1)
    IF LEN(s1) = 0 THEN
        BEEP
        MSGBOX "User ID not specified",,$AppTitle
        CONTROL SET FOCUS CB.HNDL, %ID_NEW_RFID_TEXTBOX
        EXIT FUNCTION
    END IF

    ' Name not specified
    CONTROL GET TEXT CB.HNDL, %id_FirstName TO s1
    IF LEN(s1) = 0 THEN
        BEEP
        MSGBOX "Name not specified",,$AppTitle
        CONTROL SET FOCUS CB.HNDL, %id_FirstName
        EXIT FUNCTION
    END IF

    IF lNameError=1 THEN
        lNameError=0
        MSGBOX "Error in Name",,$AppTitle
        CONTROL SET FOCUS CB.HNDL, %id_FirstName
        EXIT FUNCTION
    END IF

    ' Telephone number not specified
    CONTROL GET TEXT CB.HNDL, %ID_MOBILE TO s1
    IF LEN(s1) = 0 THEN
        BEEP
        MSGBOX "Mobile number not specified",,$AppTitle
        CONTROL SET FOCUS CB.HNDL, %ID_MOBILE
        EXIT FUNCTION
    END IF


    ' Now determine if ADD or UPDATE buttons clicked
    IF CB.CTL = %id_Add OR gMaxEntries < 1 THEN     ' It's the Add button, or we have no existing entries

    CONTROL GET TEXT CB.HNDL,%ID_NEW_RFID_TEXTBOX TO s1
    s1=TrimRFID(s1)            ' incoming RFID string may have more characters. take first 8 characters only
    s1=TRIM$(UCASE$(s1))
        IF CheckifDuplicate(s1,gMaxEntries) THEN
            MSGBOX "Another record with same ID number can not be added",,$AppTitle
            EXIT FUNCTION
        ELSE
            INCR gMaxEntries
            gCurrentEntry = gMaxEntries
        END IF
    ELSE                        ' It's the update button
        ' Simply update the existing record
    END IF

    ' Get the data from the dialog
    xDialogToUdt CB.HNDL, gCurrentEntry

    ' Reset the modified flag
    gIsRecModified = %FALSE

    ' Flag data as modified
    gIsFileModified = %TRUE

    ' Update the dialog controls
    xSetButtonStatus CBHNDL

END FUNCTION

'-----------------------------------------------------------------------------
'FUNCTION CheckifDuplicate(sID AS STRING) AS LONG
' checks if same RFID is found in farmedatabase
'input: RFID number tobe checked, number of records to be checked
'eturns : number>0 if same number aleady present in database
'         0 if same number nit present
'-----------------------------------------------------------------------------
FUNCTION CheckifDuplicate(sID1 AS STRING,lMax AS LONG) AS LONG
LOCAL l1,l2,l3 AS LONG
LOCAL s1 ,s2 AS STRING
s2=sID1
l1=LEN(s2)
IF l1>8 THEN
    s2=LEFT$(s2,8)
END IF

l3=0
FOR l1=1 TO lMax
    s1=FarmerData(l1).sIDnumber
'    msgbox "Searching for :"+s2+" Found "+s1,,$AppTitle
    s1=TRIM$(UCASE$(s1))
    IF s1=s2 THEN
        l3=l1
        EXIT FOR
    END IF
NEXT l1
FUNCTION=l3
END FUNCTION

'------------------------------------------------------------------------------
' Find button callback function
'------------------------------------------------------------------------------
CALLBACK FUNCTION xFindCallBack

    STATIC hDlg   AS DWORD
    LOCAL  x      AS LONG

    ' Is this dialog already active?
    IF hDlg THEN
        ' Yes, switch focus to the edit control in the Find dialog
        CONTROL SET FOCUS hDlg, %id_Find
        EXIT FUNCTION
    END IF

    ' Create our "Find" dialog as a modeless dialog
    DIALOG NEW CB.HNDL, " Find Record", 0, 0, 150, 50, _
               %DS_MODALFRAME OR %WS_CAPTION OR %DS_CENTER, _
               %WS_EX_TOOLWINDOW TO hDlg

    IF ERR OR hDlg = 0 THEN EXIT FUNCTION  ' An Error occurred
    DIALOG SET COLOR   hDlg, -1, RGB(222,222,255)
    DIALOG SET USER hDlg, 1, CBHNDL  ' CBHNDL is main dialog = parent

    CONTROL ADD LABEL, hDlg, -1, "Find &Text:", 2, 9, 30, 10, %SS_RIGHT
    CONTROL SET COLOR  hDlg, -1, %BLUE, -2&

    CONTROL ADD TEXTBOX, hDlg, %id_Find,  gsFindText,   34,  7, 110, 13
    CONTROL ADD BUTTON,  hDlg, %IDOK,     "&Find Next", 34, 30,  40, 14, %BS_DEFAULT
    CONTROL ADD BUTTON,  hDlg, %IDCANCEL, "&Cancel",    80, 30,  40, 14 CALL xCancelCallBack

    ' Display the dialog
    DIALOG SHOW MODELESS hDlg, CALL xFindDialogCallBack

    ' Now loop until the dialog is dismissed
    DO
        ' Allow messages to be dispatched
        DIALOG DOEVENTS

        ' Query the dialog size... if the return value is zero, the dialog was destroyed
        DIALOG GET SIZE hDlg TO x, x
    LOOP WHILE x

    ' Reactivate our parent dialog (in case previous window in the z-order was not our
    ' main dialog)
    DIALOG SHOW STATE CB.HNDL, %SW_SHOWNORMAL

    ' Clear static dialog handle on exit
    hDlg = 0

END FUNCTION


'------------------------------------------------------------------------------
' Find Dialog callback function
'------------------------------------------------------------------------------
CALLBACK FUNCTION xFindDialogCallback

    LOCAL x        AS LONG
    LOCAL hParent  AS DWORD
    LOCAL sFindText AS STRING

    SELECT CASE CBMSG

    CASE %WM_COMMAND
        SELECT CASE AS LONG CB.CTL
        CASE %id_Find  ' Text field
            ' Retrieve the search text if the edit control contents change.
            ' We save it in a global string variable so we can recreate the
            ' Find dialog with the same search text if it is relaunched again.
            IF CB.CTLMSG = %EN_UPDATE THEN  ' The text was changed
                ' Get the text and trim away eventual leading/trailing spaces
                CONTROL GET TEXT CB.HNDL, CBCTL TO gsFindText
                gsFindText = TRIM$(gsFindText)
            END IF

        CASE %IDOK  ' Ok button
            ' Now initiate a search as long as the search text is available
            IF CB.CTLMSG = %BN_CLICKED AND LEN(gsFindText) THEN
                DIALOG GET USER CB.HNDL, 1 TO hParent  ' get main dialog's handle

                ' Start search from next record and loop from end to start
                sFindText = UCASE$(gsFindText)
                FOR x = gCurrentEntry + 1 TO gMaxEntries
                    IF INSTR(UCASE$(FarmerData(x).sFirstName), sFindText) THEN
                        gIsRecModified = %FALSE
                        gCurrentEntry = x
                        xUdtToDialog hParent, gCurrentEntry
                        xSetButtonStatus hParent
                        EXIT FUNCTION
                    END IF
                NEXT

                ' No match between index and end, so restart from beginning of file
                FOR x = 1 TO gCurrentEntry
                    IF INSTR(UCASE$(FarmerData(x).sFirstName), sFindText) THEN
                        gIsRecModified = %FALSE
                        gCurrentEntry = x

                        xUdtToDialog hParent, gCurrentEntry
                        xSetButtonStatus hParent
                        EXIT FUNCTION
                    END IF
                NEXT

                ' Hmmm... no matches at all!
                ' We are not using a DDT dialog here... we could, but we'll just
                ' do this simply for demonstration purposes. By disabling the Modeless
                ' Find dialog while the MSGBOX is shown, we ensure the parent dialog
                ' keeps its zOrder and regains focus when the Find dialog is closed.
                ' Else Windows may lose track of it and send it to the back..
                DIALOG DISABLE CB.HNDL
                MSGBOX "No occurrences found.", %MB_ICONERROR OR %MB_TASKMODAL, "Find record"
                DIALOG ENABLE CB.HNDL
            END IF

        END SELECT

    END SELECT

END FUNCTION


'------------------------------------------------------------------------------
' Delete button callback function
'------------------------------------------------------------------------------
CALLBACK FUNCTION xDeleteCallBack

    LOCAL hDlg   AS DWORD
    LOCAL Result AS LONG

    ' Only permit a deletion if the index is valid
    IF gCurrentEntry < 1 OR gCurrentEntry > gMaxEntries THEN EXIT FUNCTION

    ' Create the delete confirmation dialog
    DIALOG NEW CB.HNDL, "Delete record", 0, 0, 150, 50, %DS_MODALFRAME OR %WS_CAPTION OR _
        %WS_SYSMENU OR %DS_CENTER TO hDlg
    CONTROL ADD LABEL, hDlg, -1, "Are you sure you want to" & $CRLF & _
                                 "delete this record?", 10, 7, 130, 28, %SS_CENTER
    CONTROL ADD BUTTON, hDlg, %IDOK,     "&Ok",     30, 30, 40, 14 CALL xOkCallBack
    CONTROL ADD BUTTON, hDlg, %IDCANCEL, "&Cancel", 80, 30, 40, 14 CALL xCancelCallBack
    BEEP
    DIALOG SHOW MODAL hDlg TO Result

    ' Abort if the dialog was cancelled
    IF ISFALSE Result THEN EXIT FUNCTION

    ' Now perform the removal of the record and update our index variables
    ARRAY DELETE FarmerData(gCurrentEntry)
    DECR gMaxEntries
    gCurrentEntry = MIN&(gCurrentEntry, gMaxEntries)
    REDIM PRESERVE FarmerData(1000) AS GLOBAL TypFarmerData

    ' Update our status flags & refresh the window
    gIsRecModified = %FALSE
    gIsFileModified = %TRUE
    xSetButtonStatus CB.HNDL
    xUdtToDialog CB.HNDL, gCurrentEntry

END FUNCTION



'------------------------------------------------------------------------------
' "Generic" OK callback function for "messagebox" style DDT dialogs
'------------------------------------------------------------------------------
CALLBACK FUNCTION xOkCallBack

    lFarmerCount=gMaxEntries
    WriteMasterFIle
    DIALOG END CB.HNDL, 1

END FUNCTION


'------------------------------------------------------------------------------
' "Generic" CANCEL callback function for "messagebox" style DDT dialogs
'------------------------------------------------------------------------------
CALLBACK FUNCTION xCancelCallBack

    lFarmerCount=gMaxEntries
    WriteMasterFIle
    DIALOG END CB.HNDL, 0

END FUNCTION


'------------------------------------------------------------------------------
' Print callback function
'------------------------------------------------------------------------------
CALLBACK FUNCTION xPrintCallBack
    LOCAL c, x, y, x1, y1, x2, y2, ppiX, ppiY, w, w2 AS LONG
    LOCAL Temp AS typFarmerData, sText AS STRING
    LOCAL hTitleFont, hNormalFont AS LONG


    IF MSGBOX("Print this record?", _
              %MB_YESNO OR %MB_ICONQUESTION OR %MB_TASKMODAL, _
              "Print record") = %IDYES THEN

        ERRCLEAR
        XPRINT ATTACH DEFAULT, "XPrint test"
        IF ERR OR LEN(XPRINT$) = 0 THEN

            MSGBOX "No printer could be attached!", _
                   %MB_TASKMODAL, "Printer error!"

        ELSE
            XPRINT GET MARGIN TO x1, y1, x2, y2    ' Get printer margins
            XPRINT GET PPI TO ppiX, ppiY           ' Get resolution in pixels/inch
            y1 = ppiY - y1                         ' One inch from top edge
            x1 = ppiX - x1                         ' One inch from left edge
            x2 = x1 + ppiX                         ' Column 2
            Temp = FarmerData(gCurrentEntry)

            FONT NEW "Times New Roman", 18, 5 TO hTitleFont   ' Title Font
            FONT NEW "Times New Roman", 12, 1 TO hNormalFont  ' Normal Font

            '--------------------------------------------------------
            XPRINT SET FONT hTitleFont             ' Title - bold, underlined
            '--------------------------------------------------------
            sText = "Farmer Data, record  " + _   ' Title
                    FORMAT$(gCurrentEntry) & _
                    " / " & FORMAT$(gMaxEntries)
            XPRINT GET SIZE TO w, y                ' Get page width
            XPRINT TEXT SIZE sText TO w2, y        ' Get text width
            x = (w-w2) / 2                         ' Centered x-pos
            XPRINT SET POS (x, ppiY/4)             ' Set position
            XPRINT sText                           ' Print the text

            '--------------------------------------------------------
            XPRINT SET FONT hNormalFont            ' Column 1 - Bold
            '--------------------------------------------------------
            CONTROL GET TEXT CB.HNDL, %id_DISTRICT TO sText  ' get country
            XPRINT TEXT SIZE (sText) TO w, y2              ' need text height

            c = 0
            FOR y = 0 TO 13
                XPRINT SET POS (x1, y1 + c * y2)
                SELECT CASE AS LONG y
                CASE  0 : XPRINT "User ID:"    : INCR c
                CASE  1 : XPRINT "Name:"       : c = c + 2
                CASE  2 : XPRINT "Address:"    : INCR c
                CASE  3 : IF LEN(TRIM$(Temp.sAddress2)) THEN INCR c
                CASE  4 : IF LEN(TRIM$(Temp.sAddress3)) THEN INCR c
                CASE  5 : XPRINT "Taluk:"       : INCR c
                CASE  6 : XPRINT "District:" : INCR c
                CASE  7 : XPRINT "Pincode:" : INCR c
                CASE  8 : 'XPRINT "Country:"    : c = c + 2
                CASE  9 : XPRINT "Landline:"      : INCR c
                CASE 10 : XPRINT "Mobile:"        : INCR c
                CASE 11 : XPRINT "Email:"      : INCR c
                CASE 12 : XPRINT "Cattle Type:"        : c = c + 3
                CASE 13 : XPRINT "Comments:"
                END SELECT
            NEXT

            '--------------------------------------------------------
            XPRINT FONT "Times New Roman", 12, 0  ' Column 2 - Normal
            '--------------------------------------------------------
            c = 0
            FOR y = 0 TO 13
                XPRINT SET POS (x2, y1 + c * y2)
                SELECT CASE AS LONG y
                CASE  1 : XPRINT RTRIM$(Temp.sFirstName) + _
                           " " + RTRIM$(Temp.sLastName) : c = c + 2
                CASE  2 : XPRINT RTRIM$(Temp.sAddress1) : INCR c
                CASE  3 : IF LEN(TRIM$(Temp.sAddress2)) THEN XPRINT RTRIM$(Temp.sAddress2) : INCR c
                CASE  4 : IF LEN(TRIM$(Temp.sAddress3)) THEN XPRINT RTRIM$(Temp.sAddress3) : INCR c
                CASE  5 : XPRINT RTRIM$(Temp.sTaluk) : INCR c
                CASE  6 : XPRINT RTRIM$(Temp.sDistrict): INCR c
                CASE  7 : XPRINT RTRIM$(Temp.sPinCode)  : INCR c
                CASE  8 : XPRINT sText                 : c = c + 2
                CASE  9 : XPRINT RTRIM$(Temp.sLandline)    : INCR c
                CASE 10 : XPRINT RTRIM$(Temp.sMobile)      : INCR c
                CASE 11 : XPRINT RTRIM$(Temp.sEmail)    : INCR c
                CASE 12 : XPRINT RTRIM$(Temp.sCattleType)      : c = c + 3
                CASE 13 : ' may contain line feeds, so we parse and print line by line
                    FOR y2 = 1 TO PARSECOUNT(Temp.sComments, $CRLF)
                        XPRINT GET POS TO x, y1
                        XPRINT SET POS (x2, y1)
                        XPRINT RTRIM$(PARSE$(Temp.sComments, $CRLF, y2))
                    NEXT
                END SELECT
            NEXT

            '--------------------------------------------------------
            XPRINT CLOSE
            '--------------------------------------------------------

            FONT END hTitleFont
            FONT END hNormalFont
        END IF

    END IF

END FUNCTION


'------------------------------------------------------------------------------
' Fill dialog fields from UDT
'------------------------------------------------------------------------------
SUB xUdtToDialog(BYVAL hDlg AS DWORD, lIndex AS LONG)

    LOCAL Temp AS typFarmerData
    LOCAL SaveState AS LONG

    IF lIndex > UBOUND(FarmerData(1)) OR lIndex < LBOUND(FarmerData(1)) THEN
        EXIT SUB
    END IF

    Temp = FarmerData(lIndex)


    ' When we change the edit control data, we generate EN_UPDATE messages...
    ' Due to the design of our GUI interface, we would end up with the ADD and UPDATE
    ' buttons becoming enabled just because we loaded a new UDT record into the dialog.
    ' To overcome this, we set the "modified" flag to %TRUE, so the main callback
    ' function ingores the "update" notication messages.

    SaveState = gIsRecModified
    gIsRecModified = %TRUE

    CONTROL SET TEXT hDlg, %id_DOR ,RTRIM$(Temp.sDOR)
    CONTROL SET TEXT hDlg, %ID_NEW_RFID_TEXTBOX , RTRIM$(Temp.sIDnumber)
    CONTROL SET TEXT hDlg, %id_FirstName , RTRIM$(Temp.sFirstName)
    CONTROL SET TEXT hDlg, %id_LastName  , RTRIM$(Temp.sLastName)
    CONTROL SET TEXT hDlg, %id_Address1  ,RTRIM$(Temp.sAddress1)
    CONTROL SET TEXT hDlg, %id_Address2  ,RTRIM$(Temp.sAddress2)
    CONTROL SET TEXT hDlg, %id_Address3  ,RTRIM$(Temp.sAddress3)
    CONTROL SET TEXT hDlg, %id_Taluk     ,RTRIM$(Temp.sTaluk)
    CONTROL SET TEXT hDlg, %id_District  ,RTRIM$(Temp.sDistrict)
    CONTROL SET TEXT hDlg, %id_PINCODE   ,RTRIM$(Temp.sPinCode)
    CONTROL SET TEXT hDlg, %id_LANDLINE  ,RTRIM$(Temp.sLandline)
    CONTROL SET TEXT hDlg, %id_MOBILE    ,RTRIM$(Temp.sMobile)
    CONTROL SET TEXT hDlg, %id_AADHAR    ,RTRIM$(Temp.sAadhar)
    CONTROL SET TEXT hDlg, %id_EMAIL     ,RTRIM$(Temp.sEmail)
    CONTROL SET TEXT hDlg, %id_COMMENTS  ,RTRIM$(Temp.sComments)
    CONTROL SET TEXT hDlg, %ID_CATNUM    ,RTRIM$(Temp.sCattleCount)
    CONTROL SET TEXT hDlg, %ID_CATTYPE   ,RTRIM$(Temp.sCattleType)


    ' Restore the previous state
    gIsRecModified = SaveState





END SUB


'------------------------------------------------------------------------------
' Fill UDT from dialog fields
'------------------------------------------------------------------------------
SUB xDialogToUdt (BYVAL hDlg AS DWORD, lIndex AS LONG)
    LOCAL s1 AS STRING

    LOCAL Temp AS typFarmerData

    IF lIndex > UBOUND(FarmerData(1)) OR lIndex < LBOUND(FarmerData(1)) THEN
        EXIT SUB
    END IF

    Temp = FarmerData(lIndex)

    CONTROL GET TEXT hDlg, %id_DOR      TO Temp.sDOR
    CONTROL GET TEXT hDlg, %ID_NEW_RFID_TEXTBOX TO Temp.sIDnumber
    s1=Temp.sIDnumber:s1=TrimRFID(s1):Temp.sIDnumber=s1
    CONTROL GET TEXT hDlg, %id_FirstName TO Temp.sFirstName
    CONTROL GET TEXT hDlg, %id_LastName  TO Temp.sLastName
    CONTROL GET TEXT hDlg, %id_Address1  TO Temp.sAddress1
    CONTROL GET TEXT hDlg, %id_Address2  TO Temp.sAddress2
    CONTROL GET TEXT hDlg, %id_Address3  TO Temp.sAddress3
    CONTROL GET TEXT hDlg, %id_Taluk     TO Temp.sTaluk
    CONTROL GET TEXT hDlg, %id_District  TO Temp.sDistrict
    CONTROL GET TEXT hDlg, %id_PINCODE   TO Temp.sPinCode
    CONTROL GET TEXT hDlg, %id_LANDLINE  TO Temp.sLandline
    CONTROL GET TEXT hDlg, %id_MOBILE    TO Temp.sMobile
    CONTROL GET TEXT hDlg, %id_AADHAR    TO Temp.sAadhar
    CONTROL GET TEXT hDlg, %id_EMAIL     TO Temp.sEmail
    CONTROL GET TEXT hDlg, %id_COMMENTS  TO Temp.sComments
    CONTROL GET TEXT hDlg, %ID_CATNUM    TO Temp.sCattleCount
    CONTROL GET TEXT hDlg, %ID_CATTYPE   TO Temp.sCattleType

    FarmerData(lIndex) = Temp

END SUB


'------------------------------------------------------------------------------
' Update the dialog controls
'------------------------------------------------------------------------------
SUB xSetButtonStatus(BYVAL hDlg AS DWORD)

     IF ISTRUE gEditButton THEN
         CONTROL ENABLE  hDlg, %id_FIND
     ELSE
         CONTROL DISABLE hDlg,%ID_FIND
     END IF

    ' If any current text is modified, make the ADD & Update buttons active
    IF ISTRUE gIsRecModified THEN
        CONTROL ENABLE  hDlg, %id_Add
        CONTROL ENABLE  hDlg, %id_Update
    ELSE
        CONTROL DISABLE hDlg, %id_Add
        CONTROL DISABLE hDlg, %id_Update
    END IF

    ' Update the Index control text
    CONTROL SET TEXT hDlg, %id_Index, FORMAT$(gCurrentEntry) & " / " & FORMAT$(gMaxEntries)

    ' Do we have any records? is not, disable the Prior/Next buttons, etc
    IF ISFALSE gMaxEntries THEN
        CONTROL DISABLE hDlg, %id_Prior
        CONTROL DISABLE hDlg, %id_Next
        'CONTROL DISABLE hDlg, %id_Find
        CONTROL DISABLE hDlg, %id_Delete
        EXIT SUB
    END IF

    ' If there are records, allow the Find/Delete buttons
    CONTROL ENABLE hDlg, %id_Find
    CONTROL ENABLE hDlg, %id_Delete

    ' We have records, so set the Prior button status according to the current index position
    IF gCurrentEntry <= 1 THEN
        CONTROL DISABLE hDlg, %id_Prior
    ELSE
        CONTROL ENABLE  hDlg, %id_Prior
    END IF

    ' Update the Next control button
    IF gCurrentEntry >= gMaxEntries THEN
        CONTROL DISABLE hDlg, %id_Next
    ELSE
        CONTROL ENABLE  hDlg, %id_Next
    END IF

END SUB
'---------------------------------------------------------
'function'trimRFID(sRFID as string) as string
'---------------------------------------------------------
FUNCTION trimRFID(sRFID AS STRING) AS STRING
    LOCAL s1 AS STRING
    LOCAL l1 AS LONG

    l1=LEN(sRFID)
    IF l1>8 THEN
        s1=LEFT$(sRFID,8)
    ELSE
        s1=sRFID
    END IF
    FUNCTION=s1

END FUNCTION

'---------------------------------------------------------
'function' GetPincodePlace returns string
'---------------------------------------------------------
FUNCTION GetPincodePlace(sPincode1 AS STRING) AS STRING
LOCAL s1,s2,s3 , sPincode,sPlace,SState AS STRING
LOCAL l1,l2 , lPincodeFIle AS LONG
s3=TRIM$(sPincode1)
s1=EXE.PATH$+"PINCODELIST.CSV"
lPincodeFile=FREEFILE
ON ERROR GOTO  pincodeerror
OPEN s1 FOR INPUT AS #lPincodeFile
DO WHILE NOT EOF(#lPincodefile)
    INPUT #lPincodeFIle,sPincode,sPlace,SState
'    msgbox s3+" ,"+sPincode+","+sPlace

    IF TRIM$(sPincode)=s3 THEN
        FUNCTION=sPlace+"&"+sState
        CLOSE #lPincodeFile
        ON ERROR GOTO 0
        EXIT FUNCTION
    END IF
LOOP
pincodeError:
FUNCTION="Wrong Pincode"

CLOSE #lPincodeFile
ON ERROR GOTO 0
EXIT FUNCTION

END FUNCTION


'-----------------------------------------------------------------
'FUNCTION NameCallback () AS LONG
' sets an lNameErro flag if name is containign a non alpha character
'-----------------------------------------------------------------
FUNCTION NameCallback () AS LONG
LOCAL s1,s2 AS STRING
LOCAL l1,l2 ,l3 AS LONG

CONTROL GET TEXT hDestinationFormHandle, %id_FirstName TO s1
l2=LEN(s1)
s1=UCASE$(s1)
FOR l1= 1 TO l2
    s2=MID$(s1,l1,1)
    l3=ASC(s2)
    IF l3<65 OR l3>90 THEN
        lNameError=1
        FUNCTION=0
        EXIT FUNCTION       ' charater ascii value is outside A-Z
    END IF
NEXT l1

lNameError=0
FUNCTION=1

END FUNCTION

'-----------------------------------------------------------------
'FUNCTION PincodeCallback () AS LONG
'-----------------------------------------------------------------
FUNCTION PincodeCallback () AS LONG
LOCAL s1,s2, sName  ,s3,s4 AS STRING
LOCAL l1,l2 ,l3 AS LONG

CONTROL GET TEXT hDestinationFormHandle, %id_Pincode TO s1
l2=LEN(s1)
s1=UCASE$(s1)
s2=GetPincodePlace(s1)
l3=INSTR(s2,"&")
IF l3<>0 THEN    ' OK. place name is found
    s3=MID$(s2,1,l3-1)
    s4=MID$(s2,l3+1,l2-l3)
    CONTROL SET TEXT hDestinationFormHandle, %id_Taluk, s3
    CONTROL SET TEXT hDestinationFormHandle, %id_District, s4

ELSE
    IF l2=6 THEN            ' full pincode entered
        s3=s2
        s4=" "
        CONTROL SET TEXT hDestinationFormHandle, %id_Taluk, s3
        CONTROL SET TEXT hDestinationFormHandle, %id_District, s4

    END IF

END IF

FUNCTION=1
END FUNCTION

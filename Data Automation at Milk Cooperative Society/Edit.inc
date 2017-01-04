'------------------------------------------------------------------------------
' user interface
'------------------------------------------------------------------------------
%USEMACROS = 1
#INCLUDE ONCE "Globals.inc"

'------------------------------------------------------------------------------
' Equates
'------------------------------------------------------------------------------

' Edit Control IDs
%ID_INDEX       = 100
%ID_COMPANY     = 101
%ID_LASTNAME    = 102
%ID_FIRSTNAME   = 103
%ID_ADDRESS1    = 104
%ID_ADDRESS2    = 105
%ID_ADDRESS3    = 106
%ID_CITYNAME    = 107
%ID_STATENAME   = 108
%ID_ZIPCODE     = 109
%ID_COUNTRY     = 110
%ID_PHONE       = 111
%ID_FAX         = 112
%ID_EMAIL       = 113
%ID_URL         = 114
%ID_COMMENTS    = 115

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


'------------------------------------------------------------------------------
FUNCTION EditDataFunction() AS LONG
    ShowEditDataDIALOG()
END FUNCTION
'-----------------------------------------------------------------------------


'------------------------------------------------------------------------------
' Data structures
'------------------------------------------------------------------------------
TYPE AddressType BYTE
    Company   AS STRING * 64
    LastName  AS STRING * 32
    FirstName AS STRING * 32
    Address1  AS STRING * 64
    Address2  AS STRING * 64
    Address3  AS STRING * 64
    CityName  AS STRING * 24
    StateName AS STRING * 4
    ZipCode   AS STRING * 12
    Country   AS LONG
    Phone     AS STRING * 24
    Fax       AS STRING * 24
    Email     AS STRING * 64
    Url       AS STRING * 64
    Comments  AS STRING * 1024
END TYPE


'------------------------------------------------------------------------------
' Global data variables
'------------------------------------------------------------------------------
GLOBAL gIsRecModified  AS LONG
GLOBAL gIsFileModified AS LONG
GLOBAL gAddressBook()  AS AddressType
GLOBAL gCurrentEntry   AS LONG
GLOBAL gMaxEntries     AS LONG
GLOBAL gsFindText      AS STRING


'------------------------------------------------------------------------------
' Callback Declarations
'------------------------------------------------------------------------------
DECLARE CALLBACK FUNCTION AddressCallBack()
DECLARE CALLBACK FUNCTION PriorNextCallBack()
DECLARE CALLBACK FUNCTION AddUpdateCallBack()
DECLARE CALLBACK FUNCTION DeleteCallBack()
DECLARE CALLBACK FUNCTION OkCallBack()
DECLARE CALLBACK FUNCTION FindCallBack()
DECLARE CALLBACK FUNCTION FindDialogCallBack()
DECLARE CALLBACK FUNCTION CancelCallBack()
DECLARE CALLBACK FUNCTION PrintCallBack()


'------------------------------------------------------------------------------
' Helper Sub/Function Declarations
'------------------------------------------------------------------------------
DECLARE SUB DialogToUdt(BYVAL hDlg AS DWORD, Index AS LONG)
DECLARE SUB UdtToDialog(BYVAL hDlg AS DWORD, Index AS LONG)
DECLARE SUB SetButtonStatus(BYVAL hDlg AS DWORD)
DECLARE SUB AddCountries(BYVAL hDlg AS DWORD)


'------------------------------------------------------------------------------
' Main entry point for the application
'------------------------------------------------------------------------------
FUNCTION ShowEditDataDIALOG () AS LONG

    REGISTER x       AS LONG
    LOCAL    hDlg    AS DWORD
    LOCAL    hMenu   AS DWORD
    LOCAL    Result  AS LONG
    LOCAL    hPopup1 AS DWORD
    LOCAL    hPopup2 AS DWORD
    LOCAL    nFile   AS LONG

    '----------------------------------------------------------------
    ' Initialize data structure
    DIM gAddressBook(1 TO 1) AS GLOBAL AddressType

    ' Load our data file (if it exists)
    nFile = FREEFILE
    OPEN "ADDRESS.DAT" FOR RANDOM AS #nFile LEN = SIZEOF(gAddressBook(1))

    x = LOF(nFile) \ SIZEOF(gAddressBook(1))
    IF x THEN
        ' We have data, so we'll reinitialize the variable that track usage
        gCurrentEntry = 1
        gMaxEntries = x
        REDIM gAddressBook(1 TO gMaxEntries) AS GLOBAL AddressType

        ' Get the data
        FOR x = 1 TO gMaxEntries
            GET #nFile, x, gAddressBook(x)
        NEXT
    END IF

    CLOSE #nFile
    '----------------------------------------------------------------

    ' Create our primary GUI
    DIALOG NEW 0, $AppTitle, 77, 22, 400, 267, _
        %DS_CENTER OR %WS_CAPTION OR %WS_SYSMENU, 0 TO hDlg

    ' Abort if the dialog could not be created
    IF hDlg = 0 THEN EXIT FUNCTION  ' Error occurred

    DIALOG SET COLOR hDlg, -1&, RGB(255,255,222)

    ' Now we create our controls.  We create the static control immediately before the
    ' real control so that the keyboard accelerators switch focus to the real control.
    CONTROL ADD LABEL,    hDlg, 20,        "Rfid number", 303,   5,  60,   8, %SS_RIGHT
    CONTROL ADD LABEL,    hDlg, %id_Index,     "0 / 0", 366,   4,  25,  10, %SS_SUNKEN OR %SS_CENTER

    CONTROL ADD LABEL,    hDlg, 22,       "First Na&me",   5,  33,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_LastName,    "102",  68,  31, 150,  12

    CONTROL ADD LABEL,    hDlg, 23,           "La&st", 219,  33,  18,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_FirstName,   "103", 241,  31, 150,  12

    CONTROL ADD LABEL,    hDlg, 24,         "Addre&ss",   5,  48,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_Address1,    "104",  68,  46, 323,  12
    CONTROL ADD TEXTBOX,  hDlg, %id_Address2,    "105",  68,  61, 323,  12
    CONTROL ADD TEXTBOX,  hDlg, %id_Address3,    "106",  68,  76, 323,  12

    CONTROL ADD LABEL,    hDlg, 25,            "Ci&ty",   5,  93,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_CityName,    "107",  68,  91, 116,  12

    CONTROL ADD LABEL,    hDlg, 26,  "State/Pro&vince", 188,  93,  50,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_StateName,   "108", 241,  91,  45,  12

    CONTROL ADD LABEL,    hDlg, 27, "&Zip or Postal Code",5, 109,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_ZipCode,     "109",  68, 107,  40,  12

    CONTROL ADD LABEL,    hDlg, 28,         "Co&untry", 130, 109,  25,   8, %SS_RIGHT
    CONTROL ADD COMBOBOX, hDlg, %id_Country,     , 158, 107, 129, 100, %CBS_DROPDOWNLIST OR %WS_VSCROLL OR %WS_TABSTOP

    CONTROL ADD LABEL,    hDlg, 29,           "Ph&one",   5, 127,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_Phone,       "111",  68, 125, 150,  12

    CONTROL ADD LABEL,    hDlg, 31,           "Ema&il",   5, 144,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_Email,       "113",  68, 142, 150,  12

    CONTROL ADD LABEL,    hDlg, 33,        "C&omments",   5, 160,  60,   8, %SS_RIGHT
    CONTROL ADD TEXTBOX,  hDlg, %id_Comments,    "115",  68, 158, 323,  65, %ES_AUTOHSCROLL OR %ES_AUTOVSCROLL OR %ES_MULTILINE OR %ES_WANTRETURN OR %WS_TABSTOP OR %WS_VSCROLL, %WS_EX_CLIENTEDGE

    ' Color the LABEL controls (foreground = default, background = transparent)
    FOR x = 20 TO 33
        CONTROL SET COLOR hDlg, x, -1, -2
    NEXT x


    ' Add the command buttons
    CONTROL ADD BUTTON,   hDlg, %ID_PRIOR, "<<  &Prior",  10, 233,  40,  14 CALL PriorNextCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_NEXT,   "&Next  >>",  55, 233,  40,  14 CALL PriorNextCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_FIND,      "&Find",  105, 233,  40,  14 CALL FindCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_ADD,        "&Add",  155, 233,  40,  14 CALL AddUpdateCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_UPDATE,  "U&pdate",  200, 233,  40,  14 CALL AddUpdateCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_DELETE,  "&Delete",  245, 233,  40,  14 CALL DeleteCallBack
    CONTROL ADD BUTTON,   hDlg, %ID_PRINT,    "&Print",  295, 233,  40,  14 CALL PrintCallBack
    CONTROL ADD BUTTON,   hDlg, %IDCANCEL,    "&Close",  350, 233,  40,  14 CALL CancelCallBack

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
    AddCountries hDlg

    ' Update our "scroll" buttons
    SetButtonStatus hDlg

    ' Set the 1st address from the UDT array into the GUI
    xUdtToDialog hDlg, gCurrentEntry

    ' Start up our GUI, and run until the user quits
    DIALOG SHOW MODAL hDlg, CALL AddressCallBack TO Result

    '----------------------------------------------------------------
    ' Save our data file if any changes were made
    IF gIsFileModified THEN
        ARRAY SORT gAddressBook(1) FOR gMaxEntries, _
                   FROM 1 TO LEN(gAddressBook(1).Company + gAddressBook(1).LastName)
        nFile = FREEFILE
        OPEN "ADDRESS.DAT" FOR RANDOM AS #nFile LEN = SIZEOF(gAddressBook(1))

        FOR x = 1 TO gMaxEntries
            PUT #nFile, x, gAddressBook(x)
        NEXT x

        ' Now truncate the file at the current position in case
        ' there are less records than when we began.
        SETEOF #nFile
        CLOSE #nFile
    END IF
    '----------------------------------------------------------------

END FUNCTION


'------------------------------------------------------------------------------
' Main dialog callback
'------------------------------------------------------------------------------
CALLBACK FUNCTION AddressCallBack

    LOCAL Dummy AS LONG
    LOCAL hDlg  AS DWORD

    SELECT CASE CB.MSG

    CASE %WM_COMMAND
        ' If any control is edited, we need to register this so the ADD and
        ' UPDATE buttons can be enabled.
        IF CB.CTLMSG = %EN_UPDATE _       ' *Any* edit control was changed
        OR (CB.CTL = %id_Country AND CB.CTLMSG = %CBN_SELCHANGE) THEN ' The Country combobox was changed
            ' If this is the first "change" message, we need to update the
            ' controls in the dialog box.
            IF ISFALSE gIsRecModified THEN
                gIsRecModified = %TRUE
                SetButtonStatus CBHNDL
            END IF
        END IF

        ' Now process any other %WM_COMMAND message from controls that don't
        ' have their own callback functions.
        SELECT CASE CB.CTL

        CASE %id_Menu_Help
            Dummy = SHELL("WINHELP.EXE", 1)

        CASE %id_Menu_About
            ' About dialog
            DIALOG NEW CB.HNDL, "Address book", 0, 0, 170, 60, _
                        %DS_MODALFRAME OR %WS_CAPTION OR %WS_SYSMENU OR %DS_CENTER TO hDlg
            DIALOG SET COLOR hDlg, -1&, %BLUE
            IF hDlg = 0 THEN EXIT FUNCTION  ' Error occurred
            CONTROL ADD LABEL, hDlg, -1, $AppTitle & $CRLF & $CRLF & "by PowerBASIC, Inc.", _
                        15, 10, 130, 34, %SS_CENTER
            CONTROL SET COLOR hDlg, -1, %WHITE, %BLUE
            CONTROL ADD BUTTON, hDlg, 1, "&Ok", 60, 42, 40, 14 CALL OkCallBack
            BEEP
            DIALOG SHOW MODAL hDlg
        END SELECT

    END SELECT

END FUNCTION


'------------------------------------------------------------------------------
' PRIOR/NEXT Button callback function
'------------------------------------------------------------------------------
CALLBACK FUNCTION PriorNextCallBack

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
    SetButtonStatus CB.HNDL

END FUNCTION


'------------------------------------------------------------------------------
' ADD and UPDATE Buttons callback function
'------------------------------------------------------------------------------
CALLBACK FUNCTION AddUpdateCallBack

    ' This callback only runs if gIsRecModified is %TRUE
    LOCAL hDlg      AS DWORD
    LOCAL LastName  AS STRING

    ' Get the Last Name field
    CONTROL GET TEXT CB.HNDL, %id_LastName TO LastName

    ' Check if Last Name entered, at this stage will will not check for duplicate entries
    IF LEN(LastName) = 0 THEN
        ' Display error message dialog
        DIALOG NEW CB.HNDL, $AppTitle, 0, 0, 150, 50, %DS_MODALFRAME OR %WS_CAPTION OR _
            %WS_SYSMENU OR %DS_CENTER TO hDlg
        DIALOG SET COLOR hDlg, -1&, %RED
        IF hDlg = 0 THEN EXIT FUNCTION  ' Error occurred
        CONTROL ADD LABEL, hDlg, -1, "You must specify a Last Name!", 10, 10, 130, 14, %SS_CENTER
        CONTROL SET COLOR hDlg, -1&, %WHITE, -2%
        CONTROL ADD BUTTON, hDlg, 1, "&Ok", 55, 30, 40, 14 CALL OkCallBack
        BEEP
        DIALOG SHOW MODAL hDlg

        ' Move focus to the offending control
        CONTROL SET FOCUS CB.HNDL, %id_LastName
        EXIT FUNCTION
    END IF

    ' Now determine if ADD or UPDATE buttons clicked
    IF CB.CTL = %id_Add OR gMaxEntries < 1 THEN     ' It's the Add button, or we have no existing entries
        ' Extend the UDT array and insert the new address data
        INCR gMaxEntries
        gCurrentEntry = gMaxEntries
        REDIM PRESERVE gAddressBook(1 TO gMaxEntries) AS GLOBAL AddressType
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
    SetButtonStatus CBHNDL

END FUNCTION


'------------------------------------------------------------------------------
' Find button callback function
'------------------------------------------------------------------------------
CALLBACK FUNCTION FindCallBack

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
    CONTROL ADD BUTTON,  hDlg, %IDCANCEL, "&Cancel",    80, 30,  40, 14 CALL CancelCallBack

    ' Display the dialog
    DIALOG SHOW MODELESS hDlg, CALL FindDialogCallBack

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
CALLBACK FUNCTION FindDialogCallback

    LOCAL x        AS LONG
    LOCAL hParent  AS DWORD
    LOCAL FindText AS STRING

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
                FindText = UCASE$(gsFindText)
                FOR x = gCurrentEntry + 1 TO gMaxEntries
                    IF INSTR(UCASE$(gAddressBook(x)), FindText) THEN
                        gIsRecModified = %FALSE
                        gCurrentEntry = x
                        xUdtToDialog hParent, gCurrentEntry
                        SetButtonStatus hParent
                        EXIT FUNCTION
                    END IF
                NEXT

                ' No match between index and end, so restart from beginning of file
                FOR x = 1 TO gCurrentEntry
                    IF INSTR(UCASE$(gAddressBook(x)), FindText) THEN
                        gIsRecModified = %FALSE
                        gCurrentEntry = x

                        xUdtToDialog hParent, gCurrentEntry
                        SetButtonStatus hParent
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
CALLBACK FUNCTION DeleteCallBack

    LOCAL hDlg   AS DWORD
    LOCAL Result AS LONG

    ' Only permit a deletion if the index is valid
    IF gCurrentEntry < 1 OR gCurrentEntry > gMaxEntries THEN EXIT FUNCTION

    ' Create the delete confirmation dialog
    DIALOG NEW CB.HNDL, "Delete record", 0, 0, 150, 50, %DS_MODALFRAME OR %WS_CAPTION OR _
        %WS_SYSMENU OR %DS_CENTER TO hDlg
    CONTROL ADD LABEL, hDlg, -1, "Are you sure you want to" & $CRLF & _
                                 "delete this record?", 10, 7, 130, 28, %SS_CENTER
    CONTROL ADD BUTTON, hDlg, %IDOK,     "&Ok",     30, 30, 40, 14 CALL OkCallBack
    CONTROL ADD BUTTON, hDlg, %IDCANCEL, "&Cancel", 80, 30, 40, 14 CALL CancelCallBack
    BEEP
    DIALOG SHOW MODAL hDlg TO Result

    ' Abort if the dialog was cancelled
    IF ISFALSE Result THEN EXIT FUNCTION

    ' Now perform the removal of the record and update our index variables
    ARRAY DELETE gAddressBook(gCurrentEntry)
    DECR gMaxEntries
    gCurrentEntry = MIN&(gCurrentEntry, gMaxEntries)
    REDIM PRESERVE gAddressBook(1 TO MAX&(1, gMaxEntries)) AS GLOBAL AddressType

    ' Update our status flags & refresh the window
    gIsRecModified = %FALSE
    gIsFileModified = %TRUE
    SetButtonStatus CB.HNDL
    xUdtToDialog CB.HNDL, gCurrentEntry

END FUNCTION



'------------------------------------------------------------------------------
' "Generic" OK callback function for "messagebox" style DDT dialogs
'------------------------------------------------------------------------------
CALLBACK FUNCTION OkCallBack

    DIALOG END CB.HNDL, 1

END FUNCTION


'------------------------------------------------------------------------------
' "Generic" CANCEL callback function for "messagebox" style DDT dialogs
'------------------------------------------------------------------------------
CALLBACK FUNCTION CancelCallBack

    DIALOG END CB.HNDL, 0

END FUNCTION


'------------------------------------------------------------------------------
' Print callback function
'------------------------------------------------------------------------------
CALLBACK FUNCTION PrintCallBack
    LOCAL c, x, y, x1, y1, x2, y2, ppiX, ppiY, w, w2 AS LONG
    LOCAL Temp AS AddressType, sText AS STRING
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
            Temp = gAddressBook(gCurrentEntry)

            FONT NEW "Times New Roman", 18, 5 TO hTitleFont   ' Title Font
            FONT NEW "Times New Roman", 12, 1 TO hNormalFont  ' Normal Font

            '--------------------------------------------------------
            XPRINT SET FONT hTitleFont             ' Title - bold, underlined
            '--------------------------------------------------------
            sText = "Address book, record  " + _   ' Title
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
            CONTROL GET TEXT CB.HNDL, %id_Country TO sText  ' get country
            XPRINT TEXT SIZE (sText) TO w, y2              ' need text height

            c = 0
            FOR y = 0 TO 13
                XPRINT SET POS (x1, y1 + c * y2)
                SELECT CASE AS LONG y
                CASE  0 : XPRINT "Company:"    : INCR c
                CASE  1 : XPRINT "Name:"       : c = c + 2
                CASE  2 : XPRINT "Address:"    : INCR c
                CASE  3 : IF LEN(TRIM$(Temp.Address2)) THEN INCR c
                CASE  4 : IF LEN(TRIM$(Temp.Address3)) THEN INCR c
                CASE  5 : XPRINT "City:"       : INCR c
                CASE  6 : XPRINT "State/Prov:" : INCR c
                CASE  7 : XPRINT "Zip/Postal:" : INCR c
                CASE  8 : XPRINT "Country:"    : c = c + 2
                CASE  9 : XPRINT "Phone:"      : INCR c
                CASE 10 : XPRINT "Fax:"        : INCR c
                CASE 11 : XPRINT "Email:"      : INCR c
                CASE 12 : XPRINT "Url:"        : c = c + 3
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
                CASE  0 : XPRINT RTRIM$(Temp.Company)  : INCR c
                CASE  1 : XPRINT RTRIM$(Temp.FirstName) + _
                           " " + RTRIM$(Temp.LastName) : c = c + 2
                CASE  2 : XPRINT RTRIM$(Temp.Address1) : INCR c
                CASE  3 : IF LEN(TRIM$(Temp.Address2)) THEN XPRINT RTRIM$(Temp.Address2) : INCR c
                CASE  4 : IF LEN(TRIM$(Temp.Address3)) THEN XPRINT RTRIM$(Temp.Address3) : INCR c
                CASE  5 : XPRINT RTRIM$(Temp.CityName) : INCR c
                CASE  6 : XPRINT RTRIM$(Temp.StateName): INCR c
                CASE  7 : XPRINT RTRIM$(Temp.ZipCode)  : INCR c
                CASE  8 : XPRINT sText                 : c = c + 2
                CASE  9 : XPRINT RTRIM$(Temp.Phone)    : INCR c
                CASE 10 : XPRINT RTRIM$(Temp.Fax)      : INCR c
                CASE 11 : XPRINT RTRIM$(Temp.Email)    : INCR c
                CASE 12 : XPRINT RTRIM$(Temp.Url)      : c = c + 3
                CASE 13 : ' may contain line feeds, so we parse and print line by line
                    FOR y2 = 1 TO PARSECOUNT(Temp.Comments, $CRLF)
                        XPRINT GET POS TO x, y1
                        XPRINT SET POS (x2, y1)
                        XPRINT RTRIM$(PARSE$(Temp.Comments, $CRLF, y2))
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
' Update the dialog controls
'------------------------------------------------------------------------------
SUB SetButtonStatus(BYVAL hDlg AS DWORD)

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
        CONTROL DISABLE hDlg, %id_Find
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


'------------------------------------------------------------------------------
' Prefill the ComboBox control with the names of countries
'------------------------------------------------------------------------------
SUB AddCountries(BYVAL hDlg AS DWORD)

    LOCAL x AS LONG

    ' Clear the combo box list
    COMBOBOX RESET hDlg, %id_Country
    FOR x = 1 TO DATACOUNT
        COMBOBOX ADD hDlg, %id_Country, READ$(x)
    NEXT

    DATA "United States", "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Anguilla"
    DATA "Antarctica", "Argentina", "Armenia", "Aruba", "Australia", "Austria"
    DATA "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus"
    DATA "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia", "Botswana"
    DATA "Bouvet Island", "BosniaHerzogovina", "Brazil", "Brunei", "Bulgaria"
    DATA "Burkina Faso", "Cambodia", "Cameroon", "Canada", "Cape Verde"
    DATA "Cayman Islands", "Central African Rep", "Chad", "Chile", "China"
    DATA "Christmas Island", "Colombia", "Comoros", "Congo", "Cook Islands"
    DATA "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Denmark"
    DATA "Djibouti", "Dominica", "Dominican Republic", "East Timor", "Ecuador"
    DATA "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia"
    DATA "Ethiopia", "Falkland Islands", "Faroe Islands", "Fiji", "Finland"
    DATA "France", "France Metropolitan", "French Guiana", "French Polynesia"
    DATA "French S. Territories", "Gabon", "Gambia", "Georgia", "Germany", "Ghana"
    DATA "Gibraltar", "Greece", "Greenland", "Grenada", "Guadeloupe", "Guam"
    DATA "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras"
    DATA "Hong Kong", "Hungary", "Iceland", "IN India", "Indonesia", "Iran", "Iraq"
    DATA "Ireland", "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan"
    DATA "Kazakhstan", "Kenya", "Kiribati", "Korea (North)", "Korea (South)"
    DATA "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia"
    DATA "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Macau", "Macedonia"
    DATA "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta"
    DATA "Marshall Islands", "Martinique", "Mauritania", "Mauritius", "Mayotte"
    DATA "Mexico", "Micronesia", "Miquelon", "Moldova", "Monaco", "Mongolia", "Montserrat"
    DATA "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal"
    DATA "Netherlands", "Nevis", "New Caledonia", "New Guinea", "New Zealand"
    DATA "Nicaragua", "Niger", "Nigeria", "Niue", "Norfolk Island", "Norway"
    DATA "Oman", "Pakistan", "Palau", "Panama", "Paraguay", "Peru", "Philippines"
    DATA "Pitcairn", "Poland", "Portugal", "Principe", "Puerto Rico", "Qatar"
    DATA "Reunion", "Romania", "Russian Federation", "Rwanda", "Saint Kitts"
    DATA "Saint Lucia", "Samoa", "San Marino", "Sao Tome", "Saudi Arabia", "Senegal"
    DATA "Seychelles", "Sierra Leone", "Singapore", "Slovak Republic", "Slovenia"
    DATA "Solomon Islands", "Somalia", "South Africa", "Spain", "Sri Lanka"
    DATA "St. Helena", "St. Pierre", "Sudan", "Suriname", "Swaziland", "Sweden"
    DATA "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand"
    DATA "Tobago", "Togo", "Tokelau", "Tonga", "Trinidad", "Tunisia", "Turkey"
    DATA "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates"
    DATA "United Kingdom", "United States", "Uruguay", "Uzbekistan", "Vanuatu"
    DATA "Vatican City State", "Venezuela", "Viet Nam", "Virgin Islands (BR)"
    DATA "Virgin Islands (US)", "Western Sahara", "Yemen", "Yugoslavia", "Zaire"
    DATA "Zambia", "Zimbabwe", "Other"

END SUB
'------------------------------------------------------------------------------

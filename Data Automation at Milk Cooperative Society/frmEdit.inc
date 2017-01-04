'------------------------------------------------------------------------------
' user interface
'------------------------------------------------------------------------------
%USEMACROS = 1
#INCLUDE ONCE "Globals.inc"

'------------------------------------------------------------------------------
' Equates
'------------------------------------------------------------------------------


'------------------------------------------------------------------------------
FUNCTION EditDataFunction() AS LONG
    ShowNewDataDIALOG()
END FUNCTION
'-----------------------------------------------------------------------------






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
' Prefill the ComboBox control with the names of countries
'------------------------------------------------------------------------------
SUB AddCountries(BYVAL hDlg AS DWORD)

    LOCAL x AS LONG

    ' Clear the combo box list
'    COMBOBOX RESET hDlg, %id_Country
'    FOR x = 1 TO DATACOUNT
'        COMBOBOX ADD hDlg, %id_Country, READ$(x)
'    NEXT

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

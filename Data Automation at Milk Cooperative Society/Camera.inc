'Camera.inc
' modified from  AVICAP example
' uses first video source only.
' Chris Holbrook 11 Mar 2012
'
#INCLUDE ONCE "win32api.inc"
'#option ansiapi
%WW = 640
%WH = 480

' MESSAGE IDs

'Setup some defines you need to connect to a webcam
'You can put these in a header
'If you are using Microsoft Visual Studio you wont need them as they
'Are included in Microsoft's version of vfw.h
%WM_CAP_START                   = %WM_USER   '&h0400
%WM_CAP_DRIVER_CONNECT          = %WM_CAP_START + 10
%WM_CAP_DRIVER_DISCONNECT       = %WM_CAP_START + 11
%WM_CAP_EDIT_COPY               = %WM_CAP_START + 30
%WM_CAP_DLG_VIDEOFORMAT         = %WM_CAP_START + 41
%WM_CAP_DLG_VIDEOSOURCE         = %WM_CAP_START + 42
%WM_CAP_SET_PREVIEW             = %WM_CAP_START + 50
%WM_CAP_SET_PREVIEWRATE         = %WM_CAP_START + 52
%WM_CAP_SET_SCALE               = %WM_CAP_START + 53
%WM_CAP_GRAB_FRAME              = %WM_CAP_START + 60
%WM_CAP_STOP                    = %WM_CAP_START+ 68


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

'--------------------------------------------------------
FUNCTION CameraON(hCap AS LONG) AS LONG
SendMessage(hCap, %WM_CAP_DRIVER_CONNECT,  0,      0)
SLEEP 100
SendMessage(hCap, %WM_CAP_SET_SCALE,       %true , 0)
SendMessage(hCap, %WM_CAP_SET_PREVIEWRATE, 66,     0)
SendMessage(hCap, %WM_CAP_SET_PREVIEW,     %true , 0)
SendMessage(hCap, %WM_CAP_DLG_VIDEOSOURCE, 0, 0)

END FUNCTION

'--------------------------------------------------------
FUNCTION CameraOFF(hCap AS LONG) AS LONG
sendmessage hCap, %WM_CAP_DRIVER_DISCONNECT, 0, 0
END FUNCTION
'--------------------------------------------------------
FUNCTION SetCameraWindow(hCap AS LONG) AS LONG
' assumption - the video source has index = 0
SendMessage hCap, %WM_CAP_DRIVER_CONNECT, 0, 0
SLEEP 100
SendMessage hCap, %WM_CAP_SET_SCALE, %True, 0
SendMessage hCap, %WM_CAP_SET_PREVIEWRATE, 30, 0
SendMessage hCap, %WM_CAP_SET_PREVIEW, %True, 0
END FUNCTION

'-------------------------------------------------------

FUNCTION SaveImage_GDIP(sJpgFileName AS STRING, targetHandle AS DWORD, targetID AS LONG) AS LONG
   'with Graphic Control   targetHandle is hDlg and %IDC_Graphic1
   'with Memory Bitmap     targetHandle is hBMP and 0
   'with Graphic Window    targetHandle is hWin and 0
   LOCAL hr AS LONG, token AS DWORD, hDC AS DWORD
   LOCAL hStatus AS LONG, pGraphics AS DWORD, pImage AS DWORD
   LOCAL StartupInput AS GdiplusStartupInput

   ' Initialize GDI+
   StartupInput.GdiplusVersion = 1
   hr = GdiplusStartup(token, StartupInput, BYVAL %NULL)
   IF hr THEN FUNCTION = 0 : EXIT FUNCTION   'return 0 if fails

   GRAPHIC ATTACH targetHandle, targetID     ' Select the drawing target
   GRAPHIC GET DC TO hdc                     ' Retrieve the handle of the device context
   GRAPHIC COLOR %BLACK, %WHITE              ' Set the foreground and background color
   GRAPHIC CLEAR                             ' Clear the entire selected graphic window


'***
'  At this point the image is available to both GDI+ and DDT manipulations
'  Here's an attempt to save the loaded image to a JPG file.
   LOCAL s AS STRING, EncoderClsid AS GUID, OutFileName AS STRING
   s = GdiPlusGetEncoderClsid("image/jpeg")
   EncoderClsid = GUID$(s)
   OutFileName = UCODE$(sJpgFileName) + $NUL
   GdipSaveImageToFile pImage, STRPTR(OutFileName), EncoderClsid, BYVAL %NUll
'***

   IF pImage THEN GdipDisposeImage(pImage)                      ' Cleanup
   IF pGraphics THEN GdipDeleteGraphics(pGraphics)              ' Cleanup
   GdiplusShutdown token                                        ' Shutdown GDI+

   FUNCTION = 1                                                 ' Success
END FUNCTION


FUNCTION GdiPlusGetEncoderClsid (BYVAL strMimeType AS STRING) AS STRING

   LOCAL hr AS LONG
   LOCAL pImageCodecInfo AS ImageCodecInfo PTR
   LOCAL numEncoders AS DWORD
   LOCAL nSize AS DWORD
   LOCAL i AS LONG
   LOCAL wstrlen AS LONG
   LOCAL sMimeType AS STRING

   hr = GdipGetImageEncodersSize(numEncoders, nSize)
   REDIM buffer(nSize - 1) AS BYTE
   pImageCodecInfo = VARPTR(buffer(0))
   hr = GdipGetImageEncoders(numEncoders, nSize, BYVAL pImageCodecInfo)
   IF hr = 0 THEN
      FOR i = 1 TO numEncoders
         wstrlen = lstrlenW(BYVAL @pImageCodecInfo.MimeType)
         IF wstrlen THEN sMimeType = ACODE$(PEEK$(@pImageCodecInfo.MimeType, wstrlen * 2))
         IF INSTR(UCASE$(sMimeType), UCASE$(strMimeType)) THEN
            FUNCTION = GUIDTXT$(@pImageCodecInfo.Clsid)
            EXIT FOR
         END IF
         INCR pImageCodecInfo       '// Increments pointer
      NEXT
   END IF

END FUNCTION

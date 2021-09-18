; IMPORTANT INFO ABOUT GETTING STARTED: Lines that start with a
; semicolon, such as this one, are comments.  They are not executed.

; This script has a special filename and path because it is automatically
; launched when you run the program directly.  Also, any text file whose
; name ends in .ahk is associated with the program, which means that it
; can be launched simply by double-clicking it.  You can have as many .ahk
; files as you want, located in any folder.  You can also run more than
; one ahk file simultaneously and each will get its own tray icon.

; SAMPLE HOTKEYS: Below are two sample hotkeys.  The first is Win+Z and it
; launches a web site in the default browser.  The second is Control+Alt+N
; and it launches a new Notepad window (or activates an existing one).  To
; try out these hotkeys, run AutoHotkey again, which will load this file.
;-------------------------------------------------------------------------

;--------------------------------------------------------------------
; LEGEND
;
; # = windows key
; ^ = Ctrl
; + = Shift
;
;--------------------------------------------------------------------


;--------------------------------------------------------------------
; Basic stuff
;--------------------------------------------------------------------
^!+h::Reload ; reload this script

; #z::Run www.autohotkey.com
; #z::Run https://qalogin.gs.com

#g::Run https://www.google.com

;^/::Send {AppsKey}
^\::Send {AppsKey}

; Open a command prompt
#c::
	Run %ComSpec%
	Sleep, 300
	Send f:{enter}cd \{enter}
return
;--------------------------------------------------------------------


;--------------------------------------------------------------------
; Note: From now on whenever you run AutoHotkey directly, this script
; will be loaded.  So feel free to customize it to suit your needs.

; Please read the QUICK-START TUTORIAL near the top of the help file.
; It explains how to perform common automation tasks such as sending
; keystrokes and mouse clicks.  It also explains more about hotkeys.

;------------------------------------
;
; WINDOW CONTROLS
;
;------------------------------------

; Gets the edge that the taskbar is docked to.  Returns:
;   "top"
;   "right"
;   "bottom"
;   "left"
GetTaskbarEdge() {
  WinGetPos,TX,TY,TW,TH,ahk_class Shell_TrayWnd,,,

;  MsgBox, Taskbar : TX: %TX% -- TY: %TY% -- TW: %TW% -- TH: %TH%

  if (TW = A_ScreenWidth) { ; Vertical Taskbar
    if (TY = 0) {
      return "top"
    } else {
      return "bottom"
    }
  } else { ; Horizontal Taskbar
    if (TX = 0) {
      return "left"
    } else {
      return "right"
    }
  }
}

GetScreenTop() {
  WinGetPos,TX,TY,TW,TH,ahk_class Shell_TrayWnd,,,
  TaskbarEdge := GetTaskbarEdge()

  if (TaskbarEdge = "top") {
    return TH
  } else {
    return 0
  }
}

GetScreenLeft() {
  WinGetPos,TX,TY,TW,TH,ahk_class Shell_TrayWnd,,,
  TaskbarEdge := GetTaskbarEdge()

  if (TaskbarEdge = "left") {
    return TW
  } else {
    return 0
  }
}

GetScreenWidth() {
  WinGetPos,TX,TY,TW,TH,ahk_class Shell_TrayWnd,,,
  TaskbarEdge := GetTaskbarEdge()

;MsgBox, Monitor info: ( %TX% , %TY% , %TW% , %TH%, %TaskbarEdge%, %A_ScreenWidth% )


;  if (TaskbarEdge = "top" or TaskbarEdge = "bottom")
    return A_ScreenWidth

    return A_ScreenWidth - TW
}

GetScreenHeight() {
	SysGet, MonitorWorkArea, MonitorWorkArea, 1
	;MsgBox, MWAB = %MonitorWorkAreaBottom%
	Return %MonitorWorkAreaBottom%

  WinGetPos,TX,TY,TW,TH,ahk_class Shell_TrayWnd,,,
  TaskbarEdge := GetTaskbarEdge()

; MsgBox, Monitor info: ( %TX% , %TY% , %TW% , %TH%, %TaskbarEdge%, %A_ScreenWidth%, %A_ScreenHeight% )

  if (TaskbarEdge = "top" or TaskbarEdge = "bottom") {
    SH = A_ScreenHeight - TH
    return SH
  } else {
    return A_ScreenHeight
  }
}

Max(a,b)
{
	if (a>=b)
		return a
	return b
}
Min(a,b)
{
	if (a<b)
		return a
	return b
}

Monitor_Info_Display(MonitorName, ByRef MWA)
{
    MsgBox, Monitor:`t#%A_Index%`nName:`t%MonitorName%
		. `nLeft:`t%MonitorLeft% (%MWALeft% work)
		. `nTop:`t%MonitorTop% (%MWATop% work)
		. `nRight:`t%MonitorRight% (%MWARight% work)
		. `nBottom:`t%MonitorBottom% (%MWABottom% work)
}


Monitor_Stats_Display()
{
	SysGet, Num_Monitors, MonitorCount ; Gets number of display monitors
	MsgBox, There are %Num_Monitors% monitor(s)
	Loop, %Num_Monitors%
	{
	    SysGet, MonitorName, MonitorName, %A_Index%
	    SysGet, Monitor, Monitor, %A_Index%
	    SysGet, MWA, MonitorWorkArea, %A_Index%

	if( false )
	    Monitor_Info_Display(MonitorName, MWA)

	    MsgBox, Monitor:`t#%A_Index%`nName:`t%MonitorName%
			. `nLeft:`t%MonitorLeft% (%MWALeft% work)
			. `nTop:`t%MonitorTop% (%MWATop% work)
			. `nRight:`t%MonitorRight% (%MWARight% work)
			. `nBottom:`t%MonitorBottom% (%MWABottom% work)
	}
}

Which_Monitor_am_I_in(x,y)
{
	SysGet, Num_Monitors, MonitorCount ; Gets number of display monitors
	Base_x := 0
	Base_y := 0
	where_am_I := 0
	Loop, %Num_Monitors%
	{
		SysGet, MWA, MonitorWorkArea, %A_Index%
		if ( (x >= MWALeft ) && (x <= MWARight ) )
		{
			where_am_I := A_Index			
		}		
	}
	MsgBox, There are %Num_Monitors% monitor(s) `n Coordinates (%x%,%y%) are in monitor number %where_am_I%
	return where_am_I
}

GetMonitorAt(x, y, default=1) 
{ 
    SysGet, m, MonitorCount 
    ; Iterate through all monitors. 
    Loop, %m% 
    {   ; Check if the window is on this monitor. 
        SysGet, Mon, Monitor, %A_Index% 
        if (	x >= MonLeft 
        	&&	x <= MonRight
        	&&	y >= MonTop
        	&&	y <= MonBottom ) 
            return A_Index 
    } 
    return default 
}


GetMonitor(hwnd := 0)
{
; Senses the Monitor of the provided window (by HWND). If no window is provided, this
; function checks the Active Window. This can be useful for determining which monitor
; the user is actively interacting with.

; With minor modification, this function could also account for vertically positioned 
; monitors.

; If no hwnd is provided, use the Active Window
	if (hwnd)
		WinGetPos, winX, winY, winW, winH, ahk_id %hwnd%
	else
		WinGetActiveStats, winTitle, winW, winH, winX, winY

	SysGet, numDisplays, MonitorCount
	SysGet, idxPrimary, MonitorPrimary

	Loop %numDisplays%
	{	SysGet, mon, MonitorWorkArea, %a_index%
	; Left may be skewed on Monitors past 1
		if (a_index > 1)
			monLeft -= 10
	; Right overlaps Left on Monitors past 1
		else if (numDisplays > 1)
			monRight -= 10
	; Tracked based on X. Cannot properly sense on Windows "between" monitors
		if (winX >= monLeft && winX < monRight)
			return %a_index%
	}
; Return Primary Monitor if can't sense
	return idxPrimary
}


Win_Move_Resize(Nx=0,Ny=0,w=0,h=0,mode="absolute",MonitorNumber="")
{
	;------------------------------------------------------------------------------------------------------------------
	SysGet, Num_Monitors, MonitorCount ; Gets number of display monitors

;	SysGet, MWA1, MonitorWorkArea, 1   ; Gets Monitor Work Area of display monitor #1
;	SysGet, MWA2, MonitorWorkArea, 2   ; Gets Monitor Work Area of display monitor #2
;	MsgBox, Monitor 1 work Area: %MWA1Left% -- Top: %MWA1Top% -- Right: %MWA1Right% -- Bottom %MWA1Bottom%.
;	MsgBox, Monitor 2 work Area: %MWA2Left% -- Top: %MWA2Top% -- Right: %MWA2Right% -- Bottom %MWA2Bottom%.

;	SysGet, Mon1, Monitor, 1 ; gets the dimensions of display monitor #1
;	SysGet, Mon2, Monitor, 2 ; gets the dimensions of display monitor #2
;	MsgBox, The monitor(s) dimensions Left: %Mon2Left% -- Top: %Mon2Top% -- Right: %Mon2Right% -- Bottom %Mon2Bottom%.
	;------------------------------------------------------------------------------------------------------------------

;listvars

	WinGetPos, X1, Y1, W1, H1, A
	ScreenX := GetScreenLeft()
	ScreenY := GetScreenTop()
	ScreenW := GetScreenWidth()  ; #### * Num_Monitors
	ScreenH := GetScreenHeight()

if (false) ; turn this to true for debugging purposes only
	MsgBox, GetScreen:
		. `n Left: %ScreenX%
		. `n Top: %ScreenY%
		. `n Right: %ScreenW%
		. `n Bottom: %ScreenH%


;    Monitor_From := GetMonitorAt(X1+1,Y1+1) 	; get monitor number based on top left corner of window
	Monitor_From := GetMonitorAt(X1+W1/2,Y1+H1/2)
;    	Monitor_From := GetMonitor() 		; get monitor number based on top left corner of window
	SysGet, MonitorName, MonitorName, %Monitor_From%

	if (MonitorNumber<>"")
	{
		Monitor_From := MonitorNumber
	}


	SysGet, MWA1, MonitorWorkArea, %Monitor_From%  ; Gets Monitor Work Area of current monitor
	Screen1X := MWA1Left
	Screen1Y := MWA1Top
	Screen1W := MWA1Right  - MWA1Left
	Screen1H := MWA1Bottom - MWA1Top

if (false) ; turn this to true for debugging purposes only
	MsgBox, Monitor work Area:
		. `n Left: %MWA1Left%
		. `n Top: %MWA1Top%
		. `n Right: %MWA1Right%
		. `n Bottom: %MWA1Bottom%
		. `n Screen1X: %Screen1X%
		. `n Screen1Y: %Screen1Y%
		. `n Screen1W: %Screen1W%
		. `n Screen1H: %Screen1H%


;	SysGet, DisplayMaxW, SM_CXVIRTUALSCREEN
;	SysGet, DisplayMaxH, SM_CYVIRTUALSCREEN
;	Screen1W := Min( DisplayMaxW, ScreenW )
;	Screen1H := Min( DisplayMaxH, ScreenH )

	if ( "relative" = mode )
	{
		W2 := W1 ; moving only - keep size
		H2 := H1 ; moving only - keep size

		X2 := X1 + Nx * Screen1W
		Y2 := Y1 + Ny * Screen1H
		Monitor_To := GetMonitorAt(X2, Y2, -1 )

		Monitor_To_BottomRight := GetMonitorAt(X2+W2, Y2+H2, -1 ) ; Monitor number where window bottom right corner would land
		SysGet, MWA2, MonitorWorkArea, %Monitor_To_Bottom_Right%  ; Gets Monitor Work Area of bottom right corner
		Screen2X := MWA2Left
		Screen2Y := MWA2Top
		Screen2W := MWA2Right - MWA2Left
		Screen2H := MWA2Bottom - MWA2Top

		if (-1 = Monitor_To)
		{ 	;------ Keep inside top-left monitor ------
			X2 := Max( Screen1X, X2 )
			Y2 := Max( Screen1Y, Y2 )
		}

		if ( (Monitor_From <> Monitor_To) && (X1 <> Screen1X) )
			X2 := Max( X2, Screen1X ) ; snap to monitor left edge

		if (Monitor_From <> Monitor_To && Y2 <> Screen1Y )
			Y2 := Max( Y2, Screen1Y ) ; snap to monitor top edge

		if (-1 = Monitor_To_BottomRight )
		{ 	;------ Do not move too far down-right ------
			X2 := Min( MWA2Right -W2, X2 )
			Y2 := Min( MWA2Bottom-H2, Y2 )
		}
	}

	if ( "absolute" = mode )
	{
		X2 := floor( Screen1X + Nx * Screen1W * 0.5 )
		Y2 := floor( Screen1Y + Ny * Screen1H * 0.5 )
		W2 := floor( w * Screen1W * 0.5 )
		H2 := floor( h * Screen1H * 0.5 )
	}

	if ( "resize" = mode )
	{
		X2 := X1 ; do not move upper-left corner
		Y2 := Y1 ; do not move upper-left corner

		Monitor_From_BottomRight := GetMonitorAt( X2+W1, Y2+H1, -1 ) ; Monitor number where window bottom right corner currently lands

		dW := Screen1W
		dH := Screen1H

		W2 := W1 + w * dW
		H2 := H1 + h * dH

		Monitor_To_BottomRight   := GetMonitorAt( X2+W2, Y2+H2, -1 ) ; Monitor number where window bottom right corner WOULD land

		if ( Monitor_To_BottomRight = -1 )
			Monitor_To_BottomRight := Monitor_From_BottomRight ; do not resize into non-existing monitor

		SysGet, MWA2, MonitorWorkArea, %Monitor_To_BottomRight%   ; Gets Monitor Work Area of bottom right corner
		Screen2X := MWA2Left
		Screen2Y := MWA2Top
		Screen2W := MWA2Right  - MWA2Left
		Screen2H := MWA2Bottom - MWA2Top

		if( Monitor_To_BottomRight <> Monitor_From_BottomRight )
		{
			dW := Screen2W
			dH := Screen2H

			W2 := W1 + w * dW
			H2 := H1 + h * dH
		}

		W2 := Max( 0.1*dW, W1 + w * dW )
		H2 := Max( 0.1*dH, H1 + h * dH )

		if( Monitor_To_BottomRight = Monitor_From )
		{
			W2 := Min( W2, MWA1Right - X2 )
			H2 := Min( H2, MWA1Bottom- Y2 )
		}
		else
		{
			W2 := Min( W2, MWA2Right  -X2 )
			H2 := Min( H2, MWa2Bottom -Y2 )
		}
	}

	X2 := round( X2 )
	Y2 := round( Y2 )
	W2 := round( W2 )
	H2 := round( H2 )

WinGet, Win_ID, , A
WinGet, Win_Status, MinMax, ahk_id %Win_ID%
If (Win_Status = 1)  ; the window is maximized
    WinRestore, ahk_id %Win_ID%

if (false) ; turn this to true for debugging purposes only
{
	MyMessage = Monitor work Area:
		. `n Window Status %Win_status%
		. `n Monitor_from %Monitor_From%
		. `n Left: %MWA1Left%
		. `n Top: %MWA1Top%
		. `n Right: %MWA1Right%
		. `n Bottom: %MWA1Bottom%
		. `n Screen1X: %Screen1X%
		. `n Screen1Y: %Screen1Y%
		. `n Screen1W: %Screen1W%
		. `n Screen1H: %Screen1H%
		. `n -----------------------
		. `n Monitor_to %Monitor_To_BottomRight%
		. `n Left: %MWA2Left%
		. `n Top: %MWA2Top%
		. `n Right: %MWA2Right%
		. `n Bottom: %MWA2Bottom%
		. `n Screen2X: %Screen2X%
		. `n Screen2Y: %Screen2Y%
		. `n Screen2W: %Screen2W%
		. `n Screen2H: %Screen2H%
		. `n -----------------------
		. `n Current Window - Before
		. `n win x1: %X1%
		. `n win y1: %Y1%
		. `n win w1: %W1%
		. `n win h1: %H1%
		. `n Current Window - After
		. `n win x2: %X2%
		. `n win y2: %Y2%
		. `n win w2: %W2%
		. `n win h2: %H2%
		. `n -----------------------
		. `n dW: %dW%
		. `n dH: %dH%
	MsgBox, %MyMessage%
	Clipboard := MyMessage
}

	WinMove, A, , x2, y2, w2, h2
	return
}


;#######################################################################
; Some test functions to understand how to deal with multiple monnitors
;#######################################################################
#^+S::Monitor_Stats_Display()

#^+L::WinList()
#^+Q::Window_Info()


#^+w::Which_Monitor_am_I_in(1000,1000)
#^+e::Which_Monitor_am_I_in(2000,1000)
#^+r::Which_Monitor_am_I_in(3000,1000)
#^+t::Which_Monitor_am_I_in(4000,2000)
;#######################################################################

#Numpad1::Win_Move_Resize(0,1,1,1,"absolute")
#Numpad2::Win_Move_Resize(0,1,2,1,"absolute")
#Numpad3::Win_Move_Resize(1,1,1,1,"absolute")
#Numpad4::Win_Move_Resize(0,0,1,2,"absolute")
#Numpad5::Win_Move_Resize(0,0,2,2,"absolute")
#Numpad6::Win_Move_Resize(1,0,1,2,"absolute")
#Numpad7::Win_Move_Resize(0,0,1,1,"absolute")
#Numpad8::Win_Move_Resize(0,0,2,1,"absolute")
#Numpad9::Win_Move_Resize(1,0,1,1,"absolute")


#^Numpad2::Win_Move_Resize(0,1,2,1,"absolute", 2)
#^!Numpad2::Win_Move_Resize(0,1.5,2,0.5,"absolute", 2)


#^+Down::Win_Move_Resize( 0,0, 0.0, 0.1,"resize")
#^+Up::Win_Move_Resize(   0,0, 0.0,-0.1,"resize")
#^+Left::Win_Move_Resize( 0,0,-0.1, 0.0,"resize")
#^+Right::Win_Move_Resize(0,0, 0.1, 0.0,"resize")

#^Up::Win_Move_Resize(    0 , -0.1,0,0,"relative")
#^Down::Win_Move_Resize(  0 ,  0.1,0,0,"relative")
#^Left::Win_Move_Resize( -0.1,   0,0,0,"relative")
#^Right::Win_Move_Resize( 0.1,   0,0,0,"relative")


#^!+Down::Win_Move_Resize( 0,0, 0. 0, 0.02,"resize")
#^!+Up::Win_Move_Resize(   0,0, 0.00,-0.02,"resize")
#^!+Left::Win_Move_Resize( 0,0,-0.02, 0.00,"resize")
#^!+Right::Win_Move_Resize(0,0, 0.02, 0.00,"resize")

#^!Up::Win_Move_Resize(    0   , -0.02,0,0,"relative")
#^!Down::Win_Move_Resize(  0   ,  0.02,0,0,"relative")
#^!Left::Win_Move_Resize( -0.02,  0   ,0,0,"relative")
#^!Right::Win_Move_Resize( 0.02,  0   ,0,0,"relative")


;#######################################################################
;The shortcuts below are intended for save-as operations with acrobat,
; where the file name has a pattern stored in the clipboard
;#######################################################################
#a::
Send ^+s
Sleep, 200
Send ^v
Sleep, 100
Send {Right}
;Sleep, 100
Send ^{Left}
;Sleep 100
Send {Left}
return

#s::
Send ^a^c!s
Sleep, 100
Send ^w
return
;#######################################################################


WinList()
{
	Run,taskmgr.exe,,HIDE
	DetectHiddenWindows,ON
	WinWait,ahk_class #32770
	ControlGet,Tab,Tab,,SysTabControl321
	loop,% Tab-1
	{
		ControlSend,SysTabControl321,{LEFT}
	}
	WinWait,ahk_class #32770
	ControlGet,List,List,,SysListView321
	WinClose
	DetectHiddenWindows,Off
	loop,parse,List,`n
	WinList.="`n" SubStr(A_loopfield,1,InStr(A_LoopField,A_TAB))
	Retrn:=WinList ? WinList : 0
	Msgbox %Retrn%
	Return
}

#!A:: ; <-- use the alt-a key to grab the active window's instance ID
{
	WinGet, WinID, ID, A
	MyWin = ahk_id %WinID%
	MsgBox %WinID% " " %ID% " " %MyWin%
	Return
}

;GetActiveWindowInfo()
;{
	; Returns an object containing information on the active window
	; Specifically:
	;   - X coordinate
	;   - Y coordinate
	;   - Window width
	;   - Window height
	;   - Window state (-1 = minimize, 0 = neither minimized nor maximized, 1 = maximized)
	;   - Monitor (the monitor number the center of the window lies in)
;	WinGetPos, X, Y, W, H, A
;	WinGet, windowState, MinMax, A
;	Monitor := GetMonitorAt(X + W/2, Y + H/2)
; 	return Object("X", X
;                 ,"Y", Y
;                 ,"W", W
;                 ,"H", H
;                 ,"Monitor", Monitor
;                 ,"windowState", windowState
;                 ,"newMonitor", Monitor)
;}


Get_Info() {
  	SysGet, numberOfMonitors, MonitorCount

	SysGet, MonitorWorkArea, MonitorWorkArea, 0

;	winInfo := GetActiveWindowInfo()
;	MsgBox winInfo

;	SysGet, monArea, Monitor, %A_Index%
;	MsgBox, Monitor number %A_Index%

	WinGetPos, X1, Y1, W1, H1, A
	MonitorB := GetMonitorAt(X1+W1/2,Y1+H1/2)
	Monitor := GetMonitor()
	
	WinGetPos,TX,TY,TW,TH,ahk_class Shell_TrayWnd,,,
	TaskbarEdge := GetTaskbarEdge()

	WinGetClass, WinClass, A
	WinGetTitle, WindowName, A

	MsgBox Window Information:
		. `n %WinClass%
		. `n %WindowName%
		. `n 
		. `n Coordinates: %X1%, %Y1%, %W1%, %H1%
		. `n Monitor # %Monitor%
		. `n Monitor B # %MonitorB%
		. `n
		. `n 'Monitor Info:'
		. `n %TX%, %TY%, %TW%, %TH%
		. `n TaskBar Hedge = %TaskbarEdge%
		. `n Screen Width  = %A_ScreenWidth%
		. `n Screen Height =  %A_ScreenHeight%
		. `n Monitor Work Area Bottom = %MonitorWorkAreaBottom%
		. `n Monitor Work Area    Top = %MonitorWorkAreaTop%
		. `n Monitor Work Area   Left = %MonitorWorkAreaLeft%
		. `n Monitor Work Area  Right = %MonitorWorkAreaRight%


	; WinList()

;	listvars
}

#^+i::Get_Info()


#IfWinActive ahk_class TscShellContainerClass
	^Capslock::           ; Ctrl+Caps Lock (couldn't make Ctrl+Shift+Caps Lock work for some reason)
	; Need a short sleep here for focus to restore properly.
	Sleep 50
	;MsgBox, Received Remote Desktop minimize hotkey %WindowName%   ; uncomment for debugging
	WinMinimize A    ; need A to specify Active window
   	return
#IfWinActive

#IfWinActive WhatsApp
	Enter::Send {Shift}+{Enter}
	^Enter::Send {Enter}
#IfWinActive


#IfWinActive Signal
	Enter::Send {Shift}+{Enter}
#IfWinActive


+!v::Paste_ClipText()
Paste_ClipText() {
	ClipSaved := ClipboardAll ; save clipboard
	clipboard = %clipboard%   ; Convert any copied files, HTML, or other formatted text to plain text.
	Send ^v
	Clipboard := ClipSaved    ; Restore the original clipboard. Note the use of Clipboard (not ClipboardAll).
}


Window_Info()
{
	WinGetClass, WinClass, A
	WinGetTitle, WindowName, A
	Msgbox "%WinClass%" " " "%WindowName%"
}

^CapsLock::MinimizeRemoteDesktop()
MinimizeRemoteDesktop()
{
	WinGetTitle, WindowName, A
	If InStr( WindowName, "Remote Desktop" ) > 0
		WinMinimize A
}

#^!M::AllMonitorsInfo()
AllMonitorsInfo()
{
; Example #2: This is a working script that displays info about each monitor:
SysGet, MonitorCount, MonitorCount
SysGet, MonitorPrimary, MonitorPrimary
MsgBox, Monitor Count:`t%MonitorCount%`nPrimary Monitor:`t%MonitorPrimary%
Loop, %MonitorCount%
{
    SysGet, MonitorName, MonitorName, %A_Index%
    SysGet, Monitor, Monitor, %A_Index%
    SysGet, MonitorWorkArea, MonitorWorkArea, %A_Index%
    MsgBox, Monitor:`t#%A_Index%`nName:`t%MonitorName%`nLeft:`t%MonitorLeft% (%MonitorWorkAreaLeft% work)`nTop:`t%MonitorTop% (%MonitorWorkAreaTop% work)`nRight:`t%MonitorRight% (%MonitorWorkAreaRight% work)`nBottom:`t%MonitorBottom% (%MonitorWorkAreaBottom% work)
}
}


FlashWindow()
{
	hWnd := WinActive( "A" )
	Loop 4 {
	DllCall( "FlashWindow", UInt,hWnd, Int,True )
	Sleep 500
	}
	DllCall( "FlashWindow", UInt,hWnd, Int,False )
}


IsWindowAlwaysOnTop(windowTitle)
{
	WinGet, windowStyle, ExStyle, %windowTitle%
	isWindowAlwaysOnTop := if (windowStyle & 0x8) ? false : true ; 0x8 is WS_EX_TOPMOST.
	return isWindowAlwaysOnTop
}


#^SPACE::ToggleWindowAlwaysOnTop()
ToggleWindowAlwaysOnTop()
{
	WinGetTitle, activeWindow, A
	if IsWindowAlwaysOnTop(activeWindow) {
		notificationMessage := "" . activeWindow . "" " `n is now always ON TOP."
	}
	else {
		notificationMessage := "" . activeWindow . "" " `n is NOT always on top."
	}
	Winset, Alwaysontop, , A

	Progress, zh0 fs18, %notificationMessage%
	Sleep 1000 ; Let it display for 3 seconds.
	Progress, Off
}

^+z::
	;MinimizeRemoteDesktop()
	WinActivate, Zoom
	WinRestore, Zoom
;	if (IsWindowAlwaysOnTop(Zoom) )
;	{
;		ToggleWindowAlwaysOnTop()
;	}
	Send {enter}
	Sleep 500
	Send ^v
return

^!z::
	MinimizeRemoteDesktop()
	Run, chrome.exe %ClipboardAll%
	Sleep 1000
	Send {Left}{Enter}
	Sleep 1000
return

^+!z::
	;MinimizeRemoteDesktop()
	WinActivate, Zoom
	WinRestore, Zoom

	RXO := ParseZoom( clipboard )
	url := RXO.url
	ID  := RXO.ID
	pwd := RXO.pwd

	MsgBox, "URL: " %url%

	if ( %url% <> "" )
	{
		Run, chrome.exe %url%
		Sleep 1000
		Send {Left}{Enter}
		Sleep 1000
	}
	else
	{
		WinActivate, Zoom
		Send, %ID%
		Send, {Enter}
	}

	WinGetTitle, ActiveWindow, A
	if ( ActiveWindow = "Enter meeting password" )
	{
		Sleep 100
		Send, %pwd%
		Sleep 100
		Send {Enter}
	}

	Sleep 3000
	WinGetTitle, ActiveWindow, A
	if ( ActiveWindow = "Zoom Meeting" )
	{
		Send {Enter}
	}

	Sleep 1000
	WinGetTitle, ActiveWindow, A
	if ( ActiveWindow = "Choose ONE of the audio conference options" )
	{
		Send {Enter}
	}

	Sleep 500
	WinGetTitle, ActiveWindow, A
	if ( ActiveWindow = "Zoom Meeting" )
	{
		Win_Move_Resize(0,1.5,2,0.5,"absolute", 2)
		ToggleWindowAlwaysOnTop()
	}
return


ParseZoom( text )
{
	urlR := "/(http|ftp|https):\/\/[\w-]+(\.[\w-]+)+([\w.,@?^=%&amp;:\/~+#-]*[\w@?^=%&amp;\/~+#-])?(\\? pwd=(^[a-zA-Z0-9]*))?/"

;	urlR := "O)(https?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=/]{1,256}\.[a-zA-Z0-9()]{1,6}\b[-a-zA-Z0-9()@:%_\+.~#?&//=]*)"
;	urlR := "O)(https?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=/]{1,256})"
;	urlR := urlR "\r\nMeeting ID: ([0-9 \b]{8,13})"
;	urlR := urlR "\r\n(?:Computer\s)?Password: ([0-9]{4,20})"

	MsgBox, Attempting to parse: `n %text%
	MsgBox, Regular Expression: `n %urlR%

	Success := RegExMatch( text, urlR, RXO )
	MsgBox "RXO" `n %Success%

	RetVal := {}
	if ( Success > 0 and RXO.Count = 4)
	{
		RetVal.url := RXO.Value(1)
		RetVal.ID  := RXO.Value(3)
		RetVal.pwd := RXO.Value(4)
		return, RetVal
	}
	return, RXO
}

/*
Examples

https://blackstone.zoom.us/j/587185226
Meeting ID: 587 185 226
Password: 011509

https://blackstone.zoom.us/j/378832843?pwd=
Meeting ID: 378 832 843
Computer Password: 008376

https://blackstone.zoom.us/j/378832843?pwd=WjNuRWJYWk53VWVNWlZ5VDZjeHJCdz09
Meeting ID: 378 832 843
Computer Password: 008376

https://blackstone.zoom.us/j/92468201262?pwd=Y3pIL2tEMHk2NWQ4RnNqSU4yVE9tZz09

*/

^!+R::
{
	R := ParseZoom( clipboard )
	MsgBox, %R%

For key, value in R
    MsgBox %key% = %value%
}


^!+B::
{
	Send, https://blackstone.zoom.us/j/3032122020
	Send, {Enter}
	Send, Meeting ID: 303 212 2020
}

^#t:: ; Ctrl+window++T - it will append " - AlwaysOnTop" to windows when they are AlwaysOnTop
  WinGetActiveTitle, t
  WinGet, ExStyle, ExStyle, %t%
  if (ExStyle & 0x8)
  {
    WinSet, AlwaysOnTop, Off, %t%
    WinSetTitle, %t%,, % RegexReplace(t, " - AlwaysOnTop")
  }
  else
  {
    WinSet, AlwaysOnTop, On, %t%
    WinSetTitle, %t%,, %t% - AlwaysOnTop
  }
return

MoveMouseToActiveWindow()
{
	; Move mouse to the center of the active window
	WinGetPos , , , Width, Height, A
	mousemove, (width/2), (height/2)
}


; Ctrl + Mouse Right Click
^RButton::MoveMouseToActiveWindow()


#^NumpadSub::
{
	MoveMouseToActiveWindow()
	Send, {CTRLDOWN}{WheelDown}{CTRLUP}
}
return

#^NumpadAdd::
{ 
	MoveMouseToActiveWindow()
	Send, {CTRLDOWN}{WheelUp}{CTRLUP}
}
return

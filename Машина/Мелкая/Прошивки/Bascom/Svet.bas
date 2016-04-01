




$regfile = "m8def.dat"
'$crystal = 8000000
$baud = 9600

Dim S As String * 7                                         'строка для приема длиной 6 символов (один символ - команда, два символа - байт, три символа - адрес ячейки)

Dim Cmnd As String * 1
Dim Sbyt As String * 2
Dim Byt As Byte

Dim Rbyt As Byte
Dim Hrbyt As String * 2

Dim Saddr As String * 1
Dim Addr As Integer

Dim Den As Byte
Dim Vech1 As Byte
Dim Vech2 As Byte
Dim Svet As Integer                                         'АЦП
Dim Svet2 As Byte


Dim Inchar As String * 7                                    'Uart

Const Ok = 1
Const Of = 0

Config Adc = Single , Prescaler = Auto , Reference = Avcc   'Настройка АЦП
Config Portc.2 = Output                                     'Ходовые огни
Hod Alias Portc.2
Hod = Of
Config Portc.1 = Output                                     'Ходовые огни в фарах
Fhod Alias Portc.1
Fhod = Of
Config Portc.3 = Output                                     'Габариты
Gab Alias Portc.3
Gab = Of
Config Portc.4 = Output                                     'Реле фар и габаритов
Reley Alias Portc.4
Reley = Of
Config Portb.6 = Output                                     'Фары ближний
Far Alias Portb.6
Far = Of

Config Pind.7 = Input                                       'Вход датчика генератора
Akb Alias Pind.7
Config Pind.6 = Input                                       'Вход включения зажигания
On1 Alias Pind.6
Config Pinb.0 = Input                                       'Вход ручник
Ruch Alias Pinb.0
Config Pinb.2 = Input                                       'Вход Первая Скорость
1speed Alias Pinb.2
Config Pinb.1 = Input                                       'Вход задний ход
Rev Alias Pinb.1
Config Pinb.7 = Input                                       'Вход ближний свет
Bliz Alias Pinb.7
Config Pind.5 = Input                                       'Вход дальний свет
Dal Alias Pind.5

Declare Sub Zahig
Declare Sub Nozahig
Declare Sub Gener
Declare Sub Ruchnik
Declare Sub Speed1
Declare Sub Zad
Declare Sub Vk1
Declare Sub Vk2
Declare Sub Uart
Declare Sub No
Declare Sub Ruchnoy_rejim

On Urxc Getchar

Enable Interrupts
Enable Urxc


Readeeprom Rbyt , 301
Den = Rbyt
Readeeprom Rbyt , 302
Vech1 = Rbyt
Readeeprom Rbyt , 303
Vech2 = Rbyt

Print ; "In sistem SVET V1.0"

Config Watchdog = 2048
Start Adc

Do
Reset Watchdog
Start Watchdog
Svet = Getadc(0)
Svet = Svet / 5
Svet2 = Svet + 4

Print ; "s" ; Svet
 Waitms 100


If On1 = Of Then Gosub Zahig Else Gosub Nozahig             'если зажигание включино
Loop
Return
End

Sub Uart
Do
Svet = Getadc(0)
Svet = Svet / 5
Svet2 = Svet + 4
  Reset Watchdog
  Start Watchdog
S = ""



    Stop Watchdog
Input "Comand (z, w, r, v,q, s(save), c(Info), e(exit)б h(help):" , S




      Cmnd = Left(s , 1)                                    'берем первый символ, он командный (лмбо r либо w)




Select Case Cmnd                                            'в зависимости от команды делаем следующее:
Case "w"                                                    'если скомандовали записать
Input "Data (chislo 0-210):" , S
      Sbyt = Mid(s , 1 , 3)                                 'берем второй и третий символ, это шестнадцатеричное значение байта
      Byt = Val(sbyt)                                       'перевод AA в 170  (из примера)

Input "Naznachenie (den, ve1, ve2):" , S
      Saddr = Mid(s , 1 , 3)                                'берем последние три символа - это номер ячейки, в которую пишем байт
                                          'переводим число, представленное типом string, в числовое знаение

      If Saddr = "den" Then Addr = 301

      If Saddr = "ve1" Then Addr = 302

      If Saddr = "ve2" Then Addr = 303




      If Addr = 301 Then
      Den = Byt
      Print ; "Den=" ; Byt
      Else
      Den = Den
      End If
      If Addr = 302 Then
      Vech1 = Byt
      Print ; "Sumerki= " ; Byt
      Else
      Vech1 = Vech1
      End If
      If Addr = 303 Then
      Vech2 = Byt
      Print ; "Vecher= " ; Byt
      Else
      Vech2 = Vech2
      End If

Case "s"                                                    'записываем в eeprom
      Print ; "Save:"
      Byt = Den
      Addr = 301
      Print ; "Den= " ; Byt
      Writeeeprom Byt , Addr
      Byt = Vech1
      Addr = 302
      Print ; "Sumerki= " ; Byt
      Writeeeprom Byt , Addr
      Byt = Vech2
      Addr = 303
      Print ; "Vecher= " ; Byt
      Writeeeprom Byt , Addr



Case "c"
            Print ; "INFO:"
      If Hod = 1 Then Print ; "Hodovie ogni ON" Else Print ; "Hodovie ogni OFF"
      If Fhod = 1 Then Print ; "Hodovie ogni v farah ON" Else Print ; "Hodovie ogni v farah OFF"
      If Gab = 1 Then Print ; "Gabariti ON" Else Print ; "Gabariti OFF"
      If Far = 1 Then Print ; "Fari ON" Else Print ; "Fari OFF"
      If Reley = 1 Then Print ; "Reley= ON" Else Print ; "Reley= OFF"
      If On1 = 1 Then Print ; "Zajiganie OFF" Else Print ; "Zajiganie ON"
      If Akb = 1 Then Print ; "AKB ON" Else Print ; "AKB OFF"
      If Ruch = 1 Then Print ; "Ruchnik snyat" Else Print ; "Na ruchnike"
      If 1speed = 1 Then Print ; "Nitral" Else Print ; "1 speed ON"
      If Rev = 1 Then Print ; "Nitral" Else Print ; "Zadniy hod ON"
      If Bliz = 1 Then Print ; "Bliz svet OFF" Else Print ; "Bliz svet ON"
      If Dal = 1 Then Print ; "Dalniy svet OFF" Else Print ; "Dalniy svet ON"
       Print ; "Den=" ; Den
       Print ; "Sumerki=" ; Vech1
       Print ; "Vecher=" ; Vech2
       Print ; "Svet= " ; Svet
       Print ; "Svet2= " ; Svet2


Case "r"                                                    'если скомандовали прочитать
Input "Naznachenie (den, ve1, ve2):" , S

      Print ; "znichenie eeprom"
      Saddr = Mid(s , 1 , 3)                                'берем последние три символа - это номер ячейки, в которую пишем байт
      If Saddr = "den" Then Addr = 301
      If Saddr = "ve1" Then Addr = 302
      If Saddr = "ve2" Then Addr = 303
                                               'переводим число, представленное типом string, в числовое знаение
      Readeeprom Rbyt , Addr                                'читаем
      Hrbyt = Hex(rbyt)                                     'переводим из десятичной в шестнадцатеричную (для наглядности, это удобно)
      Print "hex=" ; Hrbyt                                  'возвращаем пользователю
      Print "byt=" ; Rbyt

   Case "e"
   Print ; "Exit"
   Exit Sub
   Exit Do

   Case "h"
   Print ; "z - zapis znacheniya odnoy komandoy"
   Print ; "w - zapis znacheniya s podskazkami (user)"
   Print ; "r - chtenie zapisi iz EEPROM"
   Print ; "s - save znacheniya v EEPROM"
   Print ; "i - info na russcom dlya BASCOM"
   Print ; "c - info full dlya telefona"
   Print ; "v - ruchnoy rejim vklucheniya ustroystv"



Case "z"                                                    'если скомандовали записать
      Sbyt = Mid(s , 2 , 3)                                 'берем второй и третий символ, это шестнадцатеричное значение байта
      Byt = Val(sbyt)                                       'перевод AA в 170  (из примера)

      Saddr = Mid(s , 5 , 3)                                'берем последние три символа - это номер ячейки, в которую пишем байт
                                          'переводим число, представленное типом string, в числовое знаение

      If Saddr = "den" Then Addr = 301
      If Saddr = "ve1" Then Addr = 302
      If Saddr = "ve2" Then Addr = 303



      If Addr = 301 Then
       Den = Byt
        Print ; "Den=" ; Byt
        Else
        Den = Den
        End If
      If Addr = 302 Then
       Vech1 = Byt
        Print ; "Sumerki= " ; Byt
         Else
         Vech1 = Vech1
        End If
      If Addr = 303 Then
      Vech2 = Byt
       Print ; "Vecher= " ; Byt
       Else
       Vech2 = Vech2
        End If


Case "q"
      Print ; "Fhod OFF"
      Fhod = Of
      Print ; "Hodovie OFF"
      Hod = Of
      Reset Watchdog
      Start Watchdog
      Print ; "Fari OFF"
      Far = Of
      Waitms 200
   Reset Watchdog
   Start Watchdog
      Print ; "Gabariti OFF"
      Gab = Of
      Waitms 200
    Reset Watchdog
   Start Watchdog
      Print ; "Reley OFF"
      Reley = Of
      Print ; "OFF FULL"
      Stop Watchdog



Case "v"
Input "Ustroystva f(far), g(gab), fh(hodf), h(hod):" , S
      Saddr = S                                             'берем последние три символа - это номер ячейки, в которую пишем байт
      If Saddr = "f" Then
       Reley = Ok
       Far = Ok
       Print ; "Fari ON"
      End If
      If Saddr = "g" Then
      Reley = Ok
      Gab = Ok
      Print ; "Gabariti ON"
      End If
      If Saddr = "fh" Then
      Fhod = Ok
      Print ; "Fhod ON"
      End If
      If Saddr = "h" Then
      Hod = Ok
      Print ; "Hodovie ON"
      End If

 End Select

Loop


End Sub


'-------------------------------------------зажигание включино--------------------------------------------------
  Sub Zahig
  Reset Watchdog
  Start Watchdog
  If Akb = Ok Then
   Gosub Gener
  Else
   If Bliz = Of Then
    Reley = Ok
    Reset Watchdog
    Start Watchdog
    Far = Of
    Reset Watchdog
    Start Watchdog
    Gab = Of
    Reset Watchdog
    Start Watchdog
    Fhod = Of
    Reset Watchdog
    Start Watchdog
    Hod = Of
    Else
    Gosub No
    End If
  End If
  End Sub

'-------------------------------------------зажигание выключино-------------------------------------------------
  Sub Nozahig
  Reset Watchdog
  Start Watchdog
  If Bliz = Of Then                                         'если фары включины
    Far = Of
    Waitms 500
 Reset Watchdog
 Start Watchdog
    Reley = Ok
    Waitms 500
 Reset Watchdog
 Start Watchdog
    Gab = Of
    Waitms 500
  Reset Watchdog
 Start Watchdog
    Fhod = Of
    Waitms 500
 Reset Watchdog
 Start Watchdog
    Hod = Of
  Else
    Gosub No
  End If
  Return
  End Sub
'------------------------------------------генератор работает---------------------------------------------------
  Sub Gener
  Reset Watchdog
  Start Watchdog
  If Rev = Of Then                                          'задний ход включен
  Gosub Vk2
    Elseif Ruch = Ok Or 1speed = Of Then                    'ручник снят или первая скорость включина
    Gosub Vk1
   Else
    Gosub No
  End If
  Return
  End Sub
'--------------------------------------------вариант 1----------------------------------------------------------
  Sub Vk1
  Reset Watchdog
  Start Watchdog
  If Svet2 > Den And Svet < Den And Fhod = Of Then
      Hod = Ok
    Fhod = Of
   Reset Watchdog
   Start Watchdog
    Reley = Of
    Reset Watchdog
   Start Watchdog
    Gab = Of
   Reset Watchdog
   Start Watchdog
    Far = Of
  End If
   If Svet >= Den Then
    Hod = Ok
    Fhod = Of
   Reset Watchdog
   Start Watchdog
    Reley = Of
   Reset Watchdog
   Start Watchdog
    Gab = Of
   Reset Watchdog
   Start Watchdog
    Far = Of
   End If
   If Svet2 > Den And Svet < Den And Hod = Of Then
   Hod = Ok
    Fhod = Ok
   Reset Watchdog
   Start Watchdog
    Reley = Of
    Reset Watchdog
   Start Watchdog
    Gab = Of
   Reset Watchdog
   Start Watchdog
    Far = Of
   End If
   If Svet2 > Vech1 And Svet < Vech1 And Gab = Of Then
   Hod = Ok
    Fhod = Ok
   Reset Watchdog
   Start Watchdog
    Reley = Of
    Reset Watchdog
   Start Watchdog
    Far = Of
   Reset Watchdog
   Start Watchdog
    Gab = Of
   End If
   If Svet >= Vech1 And Svet2 < Den Then
    Hod = Ok
    Fhod = Ok
   Reset Watchdog
   Start Watchdog
    Reley = Of
    Reset Watchdog
   Start Watchdog
    Far = Of
   Reset Watchdog
   Start Watchdog
    Gab = Of
   End If


   If Svet2 > Vech1 And Svet < Vech1 And Hod = Of Then
    Hod = Ok
    Fhod = Of
    Reset Watchdog
   Start Watchdog
    Far = Of
    If Bliz = Ok Then
     Reley = Ok
     Gab = Ok
    Else
   Reset Watchdog
   Start Watchdog
         Reley = Of
          Reset Watchdog
   Start Watchdog
     Gab = Of
     End If
   End If


   If Svet2 > Vech2 And Svet < Vech2 And Far = Of Then
   Hod = Ok
    Fhod = Of
    Reset Watchdog
   Start Watchdog
    Far = Of
    If Bliz = Ok Then
     Reley = Ok
     Gab = Ok
    Else
   Reset Watchdog
   Start Watchdog
   Reley = Of
   Reset Watchdog
   Start Watchdog
     Gab = Of
     End If
   End If



   If Svet >= Vech2 And Svet2 < Vech1 Then
    Hod = Ok
    Fhod = Of
    Reset Watchdog
   Start Watchdog
    Far = Of
    If Bliz = Ok Then
     Reley = Ok
     Gab = Ok
    Else
   Reset Watchdog
   Start Watchdog
         Reley = Of
   Reset Watchdog
   Start Watchdog
     Gab = Of
     End If
   End If


   If Svet2 > Vech2 And Svet < Vech2 And Gab = Of Then
     Fhod = Of
    Hod = Of
    If Bliz = Ok Then
    Reley = Ok
    Gab = Ok
    Far = Ok
        Else
    Reset Watchdog
   Start Watchdog
     Reley = Of
    Reset Watchdog
   Start Watchdog
     Gab = Of
   Reset Watchdog
   Start Watchdog
     Far = Of
    End If
   End If


    If Svet >= 0 And Svet2 < Vech2 Then
    Fhod = Of
    Hod = Of
    If Bliz = Ok Then
    Reley = Ok
    Gab = Ok
    Far = Ok
        Else
    Reset Watchdog
   Start Watchdog
     Reley = Of
    Reset Watchdog
   Start Watchdog
     Gab = Of
   Reset Watchdog
   Start Watchdog
     Far = Of
    End If
   End If
   Return
  End Sub
'--------------------------------------------вариант 2----------------------------------------------------------
  Sub Vk2
  Reset Watchdog
  Start Watchdog
  If Svet2 > Den And Svet < Den And Fhod = Of Then
   Hod = Ok
    Fhod = Of
    Reset Watchdog
   Start Watchdog
    Gab = Of
   Reset Watchdog
   Start Watchdog
    Far = Of
   Reset Watchdog
   Start Watchdog
    Reley = Of
   End If
   If Svet >= Den Then
    Hod = Ok
    Fhod = Of
    Reset Watchdog
   Start Watchdog
    Gab = Of
   Reset Watchdog
   Start Watchdog
    Far = Of
   Reset Watchdog
   Start Watchdog
    Reley = Of
   End If
   If Svet2 > Den And Svet < Den And Hod = Of Then
    Hod = Ok
    Fhod = Ok
   Reset Watchdog
   Start Watchdog
    Far = Of
    Reset Watchdog
   Start Watchdog
    Gab = Of
   Reset Watchdog
   Start Watchdog
    Reley = Of
   End If
   If Svet2 > Vech1 And Svet < Vech1 And Gab = Of Then
   Hod = Ok
    Fhod = Ok
   Reset Watchdog
   Start Watchdog
    Far = Of
    Reset Watchdog
   Start Watchdog
    Gab = Of
   Reset Watchdog
   Start Watchdog
    Reley = Of
   End If
   If Svet >= Vech1 And Svet2 < Den Then
    Hod = Ok
    Fhod = Ok
   Reset Watchdog
   Start Watchdog
    Far = Of
    Reset Watchdog
   Start Watchdog
    Gab = Of
   Reset Watchdog
   Start Watchdog
    Reley = Of
   End If
   If Svet2 > Vech1 And Svet < Vech1 And Fhod = Of Then
   Hod = Ok
    Fhod = Ok
   Reset Watchdog
   Start Watchdog
    Far = Of
    Gab = Ok
    Reley = Ok
   End If
   If Svet >= 0 And Svet2 < Vech1 Then
    Hod = Ok
    Fhod = Ok
   Reset Watchdog
   Start Watchdog
    Far = Of
    Reley = Ok
    Gab = Ok
   End If
   Return
  End Sub
'-------------------------------------------выключить всё-------------------------------------------------------
  Sub No
  Reset Watchdog
  Start Watchdog
   Hod = Of
   Waitms 200
   Reset Watchdog
   Start Watchdog
   Fhod = Of
   Waitms 200
      Reset Watchdog
   Start Watchdog
   Far = Of
  Reset Watchdog
  Start Watchdog
   Waitms 200
   Gab = Of
      Reset Watchdog
   Start Watchdog
   Waitms 200
   Reley = Of
   Return
  End Sub
'-------------------------------------------Ручной режим--------------------------------------------------------------------

  Sub Ruchnoy_rejim
  Do
  Stop Watchdog

  If Akb = Ok Then

  Inchar = Inkey()
  Select Case Inchar
     Case "f"
     Reset Watchdog
     Start Watchdog
     Toggle Far
     Print ; "f" ; Far
     If Gab = Ok Then Reley = Ok Else Reley = Far

     Case "g"
     Reset Watchdog
     Start Watchdog
     Toggle Gab
     Print ; "g" ; Gab
     If Far = Ok Then Reley = Ok Else Reley = Gab

     Case "h"
     Toggle Hod
     Print ; "h" ; Hod

     Case "j"
     Toggle Fhod
     Print ; "j" ; Fhod

     Case "i"
     Print ; "info"
     Waitms 100
     Print ; "f" ; Far
     Waitms 100
     Print ; "g" ; Gab
     Waitms 100
     Print ; "h" ; Hod
     Waitms 100
     Print ; "j" ; Fhod
     Case "e"
       Print ; "Exit"
   Exit Sub
   Exit Do
  End Select
  Else
  Print ; "Exit"
   Exit Sub
   Exit Do
  End If
  Loop
  Return
 End Sub
'-----------------------------------------------------------------------------------------------------------------
  Getchar:
  Inchar = Inkey()


    Cmnd = Left(inchar , 1)
    Select Case Cmnd

        Case "z"
        Input , Inchar

       Saddr = Mid(inchar , 2 , 3)
                                                  'если скомандовали записать
      Sbyt = Mid(inchar , 5 , 3)                            'берем второй и третий символ, это шестнадцатеричное значение байта
      Byt = Val(sbyt)                                       'перевод AA в 170  (из примера)

                                'берем последние три символа - это номер ячейки, в которую пишем байт
                                          'переводим число, представленное типом string, в числовое знаение

      If Saddr = "den" Then Addr = 301
      If Saddr = "sum" Then Addr = 302
      If Saddr = "vec" Then Addr = 303



      If Addr = 301 Then
       Den = Byt
        Print ; "d" ; Byt
        Else
        Den = Den
        End If
      If Addr = 302 Then
       Vech1 = Byt
        Print ; "x" ; Byt
         Else
         Vech1 = Vech1
        End If
      If Addr = 303 Then
      Vech2 = Byt
       Print ; "v" ; Byt
       Else
       Vech2 = Vech2
        End If

      Case "a"
  Gosub Uart

  Case "c"
  Stop Watchdog
            Print ; "INFO:"
            Waitms 100
      If Hod = 1 Then Print ; "h1" Else Print ; "h0"
       Waitms 100
      If Fhod = 1 Then Print ; "j1" Else Print ; "j0"
       Waitms 100
      If Gab = 1 Then Print ; "g1" Else Print ; "g0"
       Waitms 100
      If Far = 1 Then Print ; "f1" Else Print ; "f0"
       Waitms 100
      If On1 = 1 Then Print ; "z0" Else Print ; "z1"
       Waitms 100
      If Akb = 1 Then Print ; "a1" Else Print ; "a0"
       Waitms 100
      If Ruch = 1 Then Print ; "r1" Else Print ; "r0"
       Waitms 100

      If 1speed = 0 Then
      Print ; "l1"
      Waitms 100
       Print ; "n0"
      Elseif Rev = 0 Then
      Print ; "y1"
       Waitms 100
       Print ; "n0"
      Else
      Print ; "n1"
      Waitms 100
       Print ; "y0"
       Waitms 100
       Print ; "l0"
      End If
       Waitms 100
       Print ; "i0"
        Waitms 100
       Print ; "t0"
        Waitms 100
       Print ; "p0"
         Waitms 100
       Print ; "d" ; Den
        Waitms 100
       Print ; "x" ; Vech1
        Waitms 100
       Print ; "v" ; Vech2
       Waitms 100
      Reset Watchdog
   Start Watchdog







  Case "q"
      Print ; "Fhod OFF"
      Fhod = Of
      Print ; "Hodovie OFF"
      Hod = Of
      Print ; "Far OFF"
      Far = Of
      Waitms 200
      Reset Watchdog
      Start Watchdog
      Print ; "Gabariti OFF"
      Gab = Of
      Waitms 200
      Print ; "Reley OFF"
      Reley = Of
      Print ; "OFF FULL"

    Case "r"
    Print ; "GO RESET"
    Goto _reset

    Case "i"
    Print ; "s"
    Gosub Ruchnoy_rejim







    Case "s"
      Print ; "zapis"
      Byt = Den
      Addr = 301
      Writeeeprom Byt , Addr
      Byt = Vech1
      Addr = 302
      Writeeeprom Byt , Addr
      Byt = Vech2
      Addr = 303
      Writeeeprom Byt , Addr

  End Select
  Return
  End
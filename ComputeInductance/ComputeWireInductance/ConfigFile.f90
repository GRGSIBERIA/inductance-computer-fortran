    module ConfigClass
        implicit none
        
        type Config
            integer :: numofCoils
            integer :: configFD
            integer :: inputFD
            integer, dimension(:), allocatable :: wireFDs
            integer, dimension(:), allocatable :: topFDs, bottomFDs
        end type
        
        interface Config
            module procedure :: init_Config
        end interface
        
    contains
    
        type(Config) function init_Config(startFD, configPath) result(this)
            character(*), intent(in) :: configPath
            integer :: startFD
            
            character*32    option
            character*256   param, topfile, bottomfile
            character*1024  line, also
            integer wireCount, coilCount, inputCount
            integer numofWires, numofCoils, numofInputs
            
            this%configFD = startFD
            startFD = startFD + 1
            
            OPEN (this%configFD, file=configPath, status="old")
            
            numofInputs = 0
            numofWires = 0
            numofCoils = 0
            
            ! ファイルの数を数える
            do
                READ (this%configFD, *, end=100) option, param
                
                if (INDEX(option, "wirefile") > 0) then
                    numofWires = numofWires + 1
                elseif (INDEX(option, "coilfile") > 0) then
                    numofCoils = numofCoils + 1
                elseif (INDEX(option, "inputfile") > 0) then
                    numofInputs = numofInputs + 1
                end if
            end do
100         continue
            
            ! ここからエラー処理
            if (numofInputs > 1) then
                PRINT *, "Please declare only one input file."
                PRINT *, "The number of inputfiles > ", numofInputs
            elseif (numofInputs <= 0) then
                PRINT *, "Please declare an inputfile."
                STOP
            end if
            ! エラー処理ここまで
            
            this%numofCoils = numofCoils
            ALLOCATE (this%wireFDs(numofWires))
            ALLOCATE (this%topFDs(numofCoils))
            ALLOCATE (this%bottomFDs(numofCoils))
            
            ! ファイルパスを読み込んでコンフィグに記帳する
            
            ! Coil上面と下面でファイルを分けてもらう方針しかないだろう^
            ! coilfile, 上面XYデータ, 下面XYデータ
            
            REWIND (this%configFD)
            
            coilCount = 1
            wireCount = 1
            inputCount = 1
            
            do
                READ (this%configFD, "(A)", end=200) line
                
                READ (line, *) option, also
                
                if (INDEX(option, "wirefile") > 0) then     ! ワイヤの読み込み
                    READ (line, *) option, param
                    OPEN (startFD, file=param, status="old")
                    this%wireFDs(wireCount) = startFD
                    wireCount = wireCount + 1
                    startFD = startFD + 1
                    
                elseif(INDEX(option, "coilfile") > 0) then  ! 上面，下面の順番で記帳
                    READ (line, *) option, topfile, bottomfile
                    OPEN (startFD, file=topfile, status="old")
                    OPEN (startFD+1, file=bottomfile, status="old")
                    this%topFDs(coilCount) = startFD
                    this%bottomFDs(coilCount) = startFD + 1
                    coilCount = coilCount + 1
                    startFD = startFD + 2
                    
                elseif(INDEX(option, "inputfile") > 0) then ! 入力ファイルの読み込み
                    READ (line, *) option, param
                    OPEN (startFD, file=param, status="old")
                    this%inputFD = startFD
                    startFD = startFD + 1
                end if
                
            end do
200         continue            
            
        end function
    end module
    
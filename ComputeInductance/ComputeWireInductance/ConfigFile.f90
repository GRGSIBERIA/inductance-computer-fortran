    module ConfigClass
        implicit none
        
        type Config
            integer :: configFD
            integer :: inputFD
            integer, dimension(:), allocatable :: wireFDs, coilFDs, frontFDs
        end type
        
        interface Config
            module procedure :: init_Config
        end interface
        
    contains
    
        type(Config) function init_Config(startFD, configPath) result(this)
            character(*), intent(in) :: configPath
            integer, intent(in) :: startFD
            
            character*32    option
            character*256   param
            integer wireCount, coilCount, frontCount, inputCount
            
            this%configFD = startFD
            startFD = startFD + 1
            
            OPEN (this%configFD, file=configPath, status="old")
            
            inputCount = 0
            wireCount = 0
            coilCount = 0
            frontCount = 0
            
            ! ファイルの数を数える
            do
                READ (startFD, *, end=100) option, param
                
                if (INDEX(option, "wirefile") > 0) then
                    wireCount = wireCount + 1
                elseif (INDEX(option, "coilfile") > 0) then
                    coilCount = coilCount + 1
                elseif (INDEX(option, "frontfile") > 0) then
                    frontCount = frontCount + 1
                elseif (INDEX(option, "inputfile") > 0) then
                    inputCount = inputCount + 1
                end if
            end do
100         continue
            
            ! ここからエラー処理
            if (frontCount /= coilCount) then
                PRINT *, "The number of coilfile does not match the number of frontfile."
                PRINT *, "Please declare the same the number of coilfile and the number of frontfile in the config file."
                STOP
            end if
            
            if (inputCount > 1) then
                PRINT *, "Please declare only one input file."
                PRINT *, "The number of inputfiles > ", inputCount
            elseif (inputCount <= 0) then
                PRINT *, "Please declare an inputfile."
                STOP
            end if
            ! エラー処理ここまで
            
            ALLOCATE (this%wireFDs(wireCount))
            ALLOCATE (this%coilFDs(coilCount))
            ALLOCATE (this%frontFDs(frontCount))
            
            ! ファイルパスを読み込んでコンフィグに記帳する
            
            ! Coilクラスを作ってheight, radius, gammaを宣言したほうがいいような気がする
        end function
    end module
    
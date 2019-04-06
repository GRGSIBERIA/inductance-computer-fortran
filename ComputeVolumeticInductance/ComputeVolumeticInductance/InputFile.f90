    module InputFileClass
    
    ! maximumNodeIdで入力データの枝切りを行う
    type InputFile
        integer numofNodes, numofElements, maximumNodeId, minimumNodeId
        double precision, dimension(:,:), allocatable :: positions  ! 3, 節点番号
        integer, dimension(:,:), allocatable :: elements            ! 4, 節点番号
        double precision, dimension(3) :: localPosition
    end type
    
    contains
    
    integer function CountNodePositionInFile(lines, part) result(count)
        implicit none
        character(*), intent(in) :: part
        
        character*128, dimension(:), intent(in) :: lines
        character*128 line
        
        integer i
        
        count = 0
        i = 0
        
        do
200         continue

            i = i + 1
            count = count + 1
            
            ! Partがきたなら次にNodeだと本体が来る
            if (INDEX(lines(i), "*Part") > 0 .and. INDEX(lines(i), part) > 0) then
                i = i + 1
                count = count + 1
                
                if (INDEX(lines(i), "*Node") > 0) then
                    goto 100        ! 本体が来る
                end if
                
            else if (INDEX(lines(i), "*Instance") > 0 .and. INDEX(lines(i), part) > 0) then
300             continue
                
                i = i + 1
                count = count + 1
                
                if (INDEX(lines(i), "*End Instance") > 0) then
                    goto 200                            ! 何もないので読み込みを繰り返す
                else if (INDEX(lines(i), "*Node") > 0) then
                    goto 100                            ! 本体が来る
                else
                    goto 300                            ! *Endもしくは*Nodeが来るまで繰り返す
                end if
                
            end if
            
        end do
100     continue
    end function
    
    subroutine GetLocalPosition(lines, part, localPosition)
        implicit none
        character*128, dimension(:), intent(in) :: lines
        character(*), intent(in) :: part
        double precision, dimension(3), intent(out) :: localPosition
        integer i
        
        localPosition = 0.0d0
        
        do i = 1, SIZE(lines)
            if (INDEX(lines(i), "*Instance") > 0 .and. INDEX(lines(i), part) > 0) then
                if (INDEX(lines(i+1), "*End Instance") > 0 .or. INDEX(lines(i+1), "*Node") > 0) then
                    goto 500
                end if
                PRINT *, lines(i+1)
                READ (lines(i+1), *) localPosition(:)
            end if
            
        end do
500     continue        
    end subroutine
    
    subroutine CountNumofNodeAndElement(lines, position, numofNodes, numofElements)
        implicit none
        integer, intent(out) :: numofNodes, numofElements
        integer, intent(in) :: position
        integer i
        character*128, dimension(:), intent(in) :: lines
        character*128 line
        
        numofNodes = 0
        numofElements = 0
        
        ! 節点数を数える
        i = position
        do
            i = i + 1
            if (INDEX(lines(i), "*Element") > 0) then
                goto 500
            end if
            numofNodes = numofNodes + 1
        end do
500     continue
        
        
        ! 要素数を数える
        do
            i = i + 1
            if (INDEX(lines(i), "*") > 0) then
                goto 600
            end if
            numofElements = numofElements + 1
        end do
600     continue        
        
    end subroutine
    
    subroutine GetNodeAndElement(lines, head, numofNodes, positions, numofElements, elements)
        implicit none
        character*128, dimension(:), intent(in) :: lines
        integer, intent(in) :: numofNodes, numofElements
        double precision, dimension(3,numofNodes), intent(out) :: positions
        integer, dimension(4,numofElements), intent(out) :: elements
        
        integer head, i, temp
        
        i = 1
        head = head + 1
        
        PRINT *, lines(head)
        do i = 1, numofNodes
            READ (lines(head), *) temp, positions(:,i)
            head = head + 1
        end do
        
        PRINT *, lines(head)
        if (INDEX(lines(head), "*Element") > 0) then
            head = head + 1
        else
            PRINT *, "*Elementが存在しない可能性があります"
        end if
        
        do i = 1, numofElements
            READ (lines(head), *) temp, elements(:,i)
            head = head + 1
        end do
    end subroutine
    
    
    ! ファイルの内容をすべてメモリ内に配置する
    subroutine GetLines(fd, lines)
        implicit none
        integer, intent(in) :: fd
        character*128, dimension(:), allocatable, intent(out) :: lines
        integer numofLines, i
        
        numofLines = 0
        
        REWIND (fd)
        do
            READ (fd, "()", end=400)
            numofLines = numofLines + 1
        end do
400     continue
        
        ALLOCATE (lines(numofLines))
        
        REWIND (fd)
        do i = 1, numofLines
            READ (fd, "(A)") lines(i)
        end do
        
    end subroutine
    
    type(InputFile) function init_InputFile(fd, part) result(this)
        implicit none
        integer, intent(in) :: fd
        character(*), intent(in) :: part
        
        character*128, dimension(:), allocatable :: lines
        
        integer nodePosition
        
        CALL GetLines(fd, lines)    ! あらかじめファイルの中身をメモリに展開する
        nodePosition = CountNodePositionInFile(lines, part)     ! partに対応する*Nodeの行番号を取得する
        
        ! 節点と要素の数を数えつつ，ローカル座標を探索する
        CALL CountNumofNodeAndElement(lines, nodePosition, this%numofNodes, this%numofElements)
        CALL GetLocalPosition(lines, part, this%localPosition)
        
        ALLOCATE (this%positions(3,this%numofNodes))
        ALLOCATE (this%elements(4,this%numofElements))
        
        ! 節点と要素を入力する
        CALL GetNodeAndElement(lines, nodePosition, this%numofNodes, this%positions, this%numofElements, this%elements)
        this%maximumNodeId = MAXVAL(this%elements)      ! 最低でも確保する必要のあるメモリを予測するために使う
        
    end function
    
    end module
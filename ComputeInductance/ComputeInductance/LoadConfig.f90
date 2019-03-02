    module Config
    implicit none
    
    ! メモ
    ! 非透磁率
    ! 鉄: 5000
    ! ニッケル: 100～600
    
    ! gammaを決定するなら，
    ! 鉄を1としたとき
    ! ニッケルは0.1程度
    
    contains
    
    ! ファイルを読み込んで，各種設定ファイルのファイルディスクリプタを取得する
    subroutine LoadConfig(confFD, inpFD, numofXYs, startFD, xyFDs, outFD)
        integer, intent(in) :: confFD
        integer, intent(in) :: inpFD
        integer, intent(in) :: outFD
        integer, intent(in) :: startFD
        integer, intent(out) :: numofXYs
        integer, dimension(:), allocatable, intent(out) :: xyFDs
        
        character*16, option
        character*256, readData
        integer i, fileId
        
        numofXYs = 0
        
        OPEN (confFD, file="config.yml", status="old")
        
        ! レポートの数を数える
        DO
            READ (confFD, *, end=1300) option, readData
            
            IF (INDEX(option, "xyfile") > 0) THEN
                numofXYs = numofXYs + 1
            END IF
        END DO
1300     continue
        
        ALLOCATE (xyFDs(numofXYs))
        
        ! 先頭に戻ってデータを入れる
        REWIND (confFD)
        fileId = 1
        
        DO
            READ (confFD, *, end=1400) option, readData
            
            ! 各ファイルを開く
            IF (INDEX(option, "xyfile") > 0) THEN
                xyFDs(fileId) = startFD + fileId - 1
                OPEN (xyFDs(fileId), file=readData, status="old")
                fileId = fileId + 1
            ELSE IF (INDEX(option, "outfile") > 0) THEN
                OPEN (outFD, file=readData, status="replace")
            ELSE IF (INDEX(option, "inpfile") > 0) THEN
                OPEN (inpFD, file=readData, status="old")
            END IF
        END DO
1400     continue
        
        CLOSE (confFD)  ! YAMLは使わないので閉じる
    end subroutine
    
    end module
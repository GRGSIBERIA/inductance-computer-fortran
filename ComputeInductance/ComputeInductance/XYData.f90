    module XYDataClass
    use AssemblyClass
    implicit none
    
    ! XYデータの集合
    public
    type XYData
        real, dimension(:), allocatable :: times                ! 時間
        integer, dimension(:), allocatable :: nodeIds           ! ノード番号
        integer, dimension(:), allocatable :: dataIds           ! 実際のノード番号
        real, dimension(:,:,:), allocatable :: displaces        ! 3次元の位置
        real, dimension(:,:,:), allocatable :: unsortDisplaces  ! 未整理の位置
    end type
    
    ! コンストラクタ宣言
    interface XYData
        module procedure init_XYData
    end interface
    
    contains
    
    ! コンストラクタ
    type(XYData) function init_XYData(numofTimes, numofData) result(this)
        implicit none
        integer, intent(in) :: numofTimes, numofData
        
        ALLOCATE (this%times(numofTimes))
        ALLOCATE (this%nodeIds(numofData))
        ALLOCATE (this%dataIds(numofData / 3))
        ALLOCATE (this%displaces(numofTimes, numofData / 3, 3)) ! numofDataはデータの総数なので，XYZで分けると3で割らないといけない
        ALLOCATE (this%unsortDisplaces(numofTimes, numofData, 3))
        
        this%nodeIds = 0
        this%unsortDisplaces = 0
        this%displaces = 0
        this%dataIds = 0
    end function
    
    ! 未ソートの座標データをソートし直す
    subroutine ConstructDisplaces(xy, asm)
        type(XYData) :: xy
        type(Assembly) :: asm
        integer numofNodeIds, numofDataIds, numofTimes
        integer, dimension(SIZE(xy%nodeIds)) :: node2dataIds
        integer timeid, nodeid, countDataId, dataid, nid, exists, upper, lower
        
        countDataId = 1
        node2dataIds = 0
        numofTimes = SIZE(xy%times)
        numofNodeIds = SIZE(xy%nodeIds)
        numofDataIds = SIZE(xy%dataIds)
        
        ! ノード番号をデータ番号へ整理する手順
        ! - データ番号が0ならば
        !   - ノード番号がデータ番号に存在しない
        !   - ノード番号をデータ番号に転記する
        DO nodeid = 1, numofNodeIds
            exists = 0
            
            IF (countDataId > numofDataIds) THEN
                GOTO 5000
            END IF
            
            IF (xy%dataIds(countDataId) == 0) THEN
                DO dataid = 1, countDataId
                    IF (xy%dataIds(dataid) == xy%nodeIds(nodeid)) THEN
                        exists = exists + 1
                    END IF
                END DO
                
                IF (exists == 0) THEN
                    xy%dataIds(countDataId) = xy%nodeIds(nodeid)
                    countDataId = countDataId + 1
                END IF
            END IF
        END DO
5000    continue
        
        ! ノード番号からデータ番号へ添字を対応付ける処理
        DO nodeid = 1, numofNodeIds
            DO dataid = 1, numofDataIds
                IF (xy%dataIds(dataid) == xy%nodeIds(nodeid)) THEN
                    node2dataIds(nodeid) = dataid
                END IF
            END DO
        END DO
        
        ! 変位を足し合わせてベクトル化する
        DO timeid = 1, numofTimes
            DO nodeid = 1, numofNodeIds
                xy%displaces(timeid, node2dataIds(nodeid), :) =&
                    xy%displaces(timeid, node2dataIds(nodeid), :) + xy%unsortDisplaces(timeid, nodeid, :)
            END DO
        END DO
        
        ! 原点座標を足し合わせる
        ! 原点データのIDと照合する
        DO dataid = 1, numofDataIds
                
            ! 照合する, 線形探索なので二分探索したほうが速いかもしれない
            DO nid = 1, SIZE(asm%nodeIds)
                IF (asm%nodeIds(nid) == xy%dataIds(dataid)) THEN
                    ! 内側に時間を置いたら十分速くなった
                    DO timeid = 1, numofTimes
                        xy%displaces(timeid, dataid, :) = asm%positions(nid, :) + xy%displaces(timeid, dataid, :)
                    END DO
                    
                END IF
            END DO
        END DO
        
        ! これでXYデータのdisplacesに実際の座標が反映された！
    end subroutine
    
    end module
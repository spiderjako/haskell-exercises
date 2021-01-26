type Fn = String
type FirstName = String
type LastName = String
type Score = Float
type DisciplineID = String
type Course = Integer
type DisciplineName = String
type CourseYear = Integer

type StudentScore = (Fn, Score, DisciplineID)
type Student = (FirstName, LastName, Fn, Course)
type Discipline = (DisciplineName, DisciplineID, CourseYear)

getStudentScoreFnAndDisciplineID (a,_,c)  = (a,c)

getStudentFirstLastName (a, b, _, _) = (a, b)
getStudentFn (_, _, c, _) = c
getStudentCourse (_,_,_,d) = d

getStudentScoreFn (a, _, _) = a
getStudentScoreScore(_,b,_) = b
getStudentScoreDisciplineID (_, _, c) = c

getDisciplineDisciplineName (a,_,_) = a
getDisciplineDisciplineID (_,b,_) = b
getDisciplineCourseYear (_,_,c) = c

filterScoresByDiscipline scores disciplineID =
    filter (\x -> getStudentScoreDisciplineID x == disciplineID) scores

getNamesByDisciplineID :: [StudentScore] -> [Student] -> DisciplineID -> [(FirstName, LastName)]
getNamesByDisciplineID scores students disciplineId =
    let filteredStudentScores = filter (\x -> getStudentScoreDisciplineID x == disciplineId) scores
        fnInDiscipline = map getStudentScoreFn filteredStudentScores
        filteredStudents = filter (\x -> getStudentFn x `elem` fnInDiscipline) students
    in map getStudentFirstLastName filteredStudents

calculateAvgGrade :: Discipline -> [StudentScore] -> Score
calculateAvgGrade discipline filteredStudentScores =
    let groupedScores = map getStudentScoreScore (filterScoresByDiscipline filteredStudentScores (getDisciplineDisciplineID discipline))
    in sum groupedScores / (fromIntegral (length groupedScores))

getResults :: [StudentScore] -> [Discipline] -> [(DisciplineName, DisciplineID, Score)]
getResults scores disciplines =
    let listOfDisciplineIDs = map getDisciplineDisciplineID disciplines
        filteredStudentScores = filter (\x -> getStudentScoreDisciplineID x `elem` listOfDisciplineIDs) scores
    in [
            (
                getDisciplineDisciplineName discipline,
                getDisciplineDisciplineID discipline,
                calculateAvgGrade discipline filteredStudentScores
            )  |
            discipline <- disciplines
        ]

-- getStudentsByDisciplineID :: [StudentScore] -> [Student] -> DisciplineID -> [(DisciplineName, DisciplineID, Score)] -> [Student]
-- getStudentsByDisciplineID scores students disciplineId avgScores =
--     let filteredStudentScores = filter (\x -> getStudentScoreDisciplineID x == disciplineId && getStudentScoreScore x > getAvgDisciplineScore avgScores disciplineId) scores
--         fnInDiscipline = map getStudentScoreFn filteredStudentScores
--     in filter (\x -> getStudentFn x `elem` fnInDiscipline) students

getAverageGradesFn :: (DisciplineName, DisciplineID, Score) -> Fn
getAverageGradesFn (fn, _, _) = fn

getAverageGradesDisciplineID :: (DisciplineName, DisciplineID, Score) -> DisciplineID
getAverageGradesDisciplineID (_, id, _) = id

getAverageGradesGrade :: (DisciplineName, DisciplineID, Score) -> Score
getAverageGradesGrade (_, _, grade) = grade

-- for every student, get all the disciplines for his year
--getAboveAvеrageStudents :: [StudentScore] [Student] [Discipline] -> [Student]
getAboveAvеrageStudents scores students disciplines =
    let averageGradesForDisciplines = getResults scores disciplines
        filteredGrades = [ 
            [
                score
                | score <- scores,
                getStudentScoreDisciplineID score == getDisciplineDisciplineID discipline,
                getAverageGradesGrade discipline < getStudentScoreScore score
            ] | discipline <- averageGradesForDisciplines]
        filteredStudents = [
            student
            | 
        ]
        
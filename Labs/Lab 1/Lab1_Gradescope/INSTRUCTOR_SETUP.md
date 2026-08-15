# Instructor setup — Lab 1

1. Create a Gradescope **Programming Assignment** worth **10 points** and enable manual grading.
2. Upload `Lab1_autograder.zip` and wait for the build to finish.
3. On **Edit Outline**, set **Autograder** to **8 points** and add **Written responses (TA graded)** worth **2 points**.
4. Give students `Lab1_student_files.zip` and the detailed Word handout.
5. Students complete `Lab1.Rmd`, knit it locally to HTML (recommended) or PDF to verify the report, and upload only `Lab1.Rmd` to Gradescope.
6. Test the instructor R Markdown solution and confirm the automatic subtotal is **8/8**.
7. The teaching assistant grades the written responses directly from the submitted `Lab1.Rmd` using the two-point rubric in the handout.
8. Link the assignment to a 10-point Canvas assignment; Canvas controls all dates.

The autograder uses `knitr::purl()` to extract executable R chunks. Written Markdown is intentionally excluded from automatic evaluation.

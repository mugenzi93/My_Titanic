My\_Titanic
================
Clement Mugenzi
9/20/2019

This dataset will be comprised of two separate .csv files, both of which
will be combined then used to predict who survives on the titanic. But
first, let’s load all packages that I will be using in this project.

# Import the Dataset

``` r
# First, we will load the train dataset.
train = read_csv("train.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   PassengerId = col_double(),
    ##   Survived = col_double(),
    ##   Pclass = col_double(),
    ##   Name = col_character(),
    ##   Sex = col_character(),
    ##   Age = col_double(),
    ##   SibSp = col_double(),
    ##   Parch = col_double(),
    ##   Ticket = col_character(),
    ##   Fare = col_double(),
    ##   Cabin = col_character(),
    ##   Embarked = col_character()
    ## )

``` r
# Second, the test dataset is loaded
test = read_csv("test.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   PassengerId = col_double(),
    ##   Pclass = col_double(),
    ##   Name = col_character(),
    ##   Sex = col_character(),
    ##   Age = col_double(),
    ##   SibSp = col_double(),
    ##   Parch = col_double(),
    ##   Ticket = col_character(),
    ##   Fare = col_double(),
    ##   Cabin = col_character(),
    ##   Embarked = col_character()
    ## )

``` r
# Then both the train and test datasets are combined into a single dataset.
full = bind_rows(train, test)
```

# Feature Engineering

``` r
# Grab title from passenger's name.
full$Title <- gsub('(.*,) | (\\..*)', '', full$Name)
# Show title counts by sex
table(pull(full, Sex), pull(full, Title))
```

    ##         
    ##          Capt. Edward Gifford Col. Archibald IV Col. John Col. John Jacob
    ##   female                    0                 0         0               0
    ##   male                      1                 1         1               1
    ##         
    ##          Col. Oberst Alfons Don. Manuel E Dona. Fermina Dr. Alfred
    ##   female                  0             0             1          0
    ##   male                    1             1             0          1
    ##         
    ##          Dr. Alice (Farnham) Dr. Arthur Jackson Dr. Ernest
    ##   female                   1                  0          0
    ##   male                     0                  1          1
    ##         
    ##          Dr. Henry William Dr. Max Dr. Washington Dr. William Edward
    ##   female                 0       0              0                  0
    ##   male                   1       1              1                  1
    ##         
    ##          Jonkheer. John George
    ##   female                     0
    ##   male                       1
    ##         
    ##          Lady. (Lucille Christiana Sutherland) ("Mrs Morgan")
    ##   female                                                    1
    ##   male                                                      0
    ##         
    ##          Major. Archibald Willingham Major. Arthur Godfrey Master. Akar
    ##   female                           0                     0            0
    ##   male                             1                     1            1
    ##         
    ##          Master. Albert Master. Alden Gates Master. Alfred Edward
    ##   female              0                   0                     0
    ##   male                1                   1                     1
    ##         
    ##          Master. Andre Master. Arthur Master. Artur Karl
    ##   female             0              0                  0
    ##   male               1              1                  1
    ##         
    ##          Master. Assad Alexander Master. Bertram Vere Master. Carl Edgar
    ##   female                       0                    0                  0
    ##   male                         1                    1                  1
    ##         
    ##          Master. Clarence Gustaf Hugo Master. Eden Leslie "Neville"
    ##   female                            0                             0
    ##   male                              1                             1
    ##         
    ##          Master. Edmond Roger Master. Edvin Rojj Felix
    ##   female                    0                        0
    ##   male                      1                        1
    ##         
    ##          Master. Eino Viljami Master. Elias Master. Eric Master. Eugene
    ##   female                    0             0            0              0
    ##   male                      1             1            1              1
    ##         
    ##          Master. Eugene Joseph Master. Filip Oscar
    ##   female                     0                   0
    ##   male                       1                   1
    ##         
    ##          Master. Frank John William "Frankie" Master. George Hugh
    ##   female                                    0                   0
    ##   male                                      1                   1
    ##         
    ##          Master. George Sibley Master. Georges Youssef Master. Gerios
    ##   female                     0                       0              0
    ##   male                       1                       1              1
    ##         
    ##          Master. Gilbert Sigvard Emanuel Master. Gosta Leonard
    ##   female                               0                     0
    ##   male                                 1                     1
    ##         
    ##          Master. Halim Gonios ("William George") Master. Harald
    ##   female                                       0              0
    ##   male                                         1              1
    ##         
    ##          Master. Harold Theodor Master. Harold Victor Master. Henry Forbes
    ##   female                      0                     0                    0
    ##   male                        1                     1                    1
    ##         
    ##          Master. Hudson Trevor Master. James William Master. John Borie
    ##   female                     0                     0                  0
    ##   male                       1                     1                  1
    ##         
    ##          Master. John Morgan Jr Master. Juha Niilo Master. Karl Thorsten
    ##   female                      0                  0                     0
    ##   male                        1                  1                     1
    ##         
    ##          Master. Marshall Brines Master. Meier Master. Michael J
    ##   female                       0             0                 0
    ##   male                         1             1                 1
    ##         
    ##          Master. Michel M Master. Paul Folke Master. Philip Frank
    ##   female                0                  0                    0
    ##   male                  1                  1                    1
    ##         
    ##          Master. Ralph Lester Master. Richard F Master. Robert Douglas
    ##   female                    0                 0                      0
    ##   male                      1                 1                      1
    ##         
    ##          Master. Seman Master. Sidney Leonard Master. Sigvard Harald Elias
    ##   female             0                      0                            0
    ##   male               1                      1                            1
    ##         
    ##          Master. Thomas Henry Master. Urho Abraham Master. Viljo
    ##   female                    0                    0             0
    ##   male                      1                    1             1
    ##         
    ##          Master. Walter John Master. Washington
    ##   female                   0                  0
    ##   male                     1                  1
    ##         
    ##          Master. William Arthur Willie"" Master. William Frederick
    ##   female                               0                         0
    ##   male                                 1                         1
    ##         
    ##          Master. William Henry Master. William Loch "William"
    ##   female                     0                              0
    ##   male                       1                              1
    ##         
    ##          Master. William Rowe Master. William Thornton II
    ##   female                    0                           0
    ##   male                      1                           1
    ##         
    ##          Miss. (Marion Ogden) Miss. Ada Miss. Adele Kiamie "Jane"
    ##   female                    1         1                         1
    ##   male                      0         0                         0
    ##         
    ##          Miss. Agda Thorilda Viktoria Miss. Agnes Miss. Albina Miss. Alice
    ##   female                            1           1            1           2
    ##   male                              0           0            0           0
    ##         
    ##          Miss. Alice Elizabeth Miss. Alice Frances Louisa
    ##   female                     1                          1
    ##   male                       0                          0
    ##         
    ##          Miss. Alice Phoebe Miss. Alicia Miss. Aloisia Miss. Amalie
    ##   female                  1            1             1            1
    ##   male                    0            0             0            0
    ##         
    ##          Miss. Amelia Miss. Amelia "Mildred" Miss. Amelie
    ##   female            1                      1            1
    ##   male              0                      0            0
    ##         
    ##          Miss. Amy Zillah Elsie Miss. Ann Elizabeth Miss. Anna
    ##   female                      1                   1          3
    ##   male                        0                   0          0
    ##         
    ##          Miss. Anna "Annie" Miss. Anna Katherine "Annie Kate"
    ##   female                  1                                 1
    ##   male                    0                                 0
    ##         
    ##          Miss. Anna Kristine Miss. Anna Sofia Miss. Anne Miss. Annie
    ##   female                   1                3          1           1
    ##   male                     0                0          0           0
    ##         
    ##          Miss. Annie Clemmer Miss. Annie Jessie "Nina" Miss. Asuncion
    ##   female                   1                         1              1
    ##   male                     0                         0              0
    ##         
    ##          Miss. Augusta Miss. Augusta Charlotta Miss. Augusta Maria
    ##   female             1                       1                   1
    ##   male               0                       0                   0
    ##         
    ##          Miss. Aurora Adelia Miss. Banoura Miss. Barbara J
    ##   female                   1             1               1
    ##   male                     0             0               0
    ##         
    ##          Miss. Beatrice Irene Miss. Berta Olivia Miss. Bertha
    ##   female                    1                  1            4
    ##   male                      0                  0            0
    ##         
    ##          Miss. Bertha E Miss. Bertha J Miss. Bridget Miss. Bridget Delia
    ##   female              1              1             2                   2
    ##   male                0              0             0                   0
    ##         
    ##          Miss. Bridget Mary Miss. Brigdet Delia
    ##   female                  1                   1
    ##   male                    0                   0
    ##         
    ##          Miss. Carla Christine Nielsine Miss. Caroline
    ##   female                              1              1
    ##   male                                0              0
    ##         
    ##          Miss. Caroline Louise Miss. Catharina
    ##   female                     1               1
    ##   male                       0               0
    ##         
    ##          Miss. Catherine Helen "Carrie" Miss. Catherine Katie""
    ##   female                              1                       1
    ##   male                                0                       0
    ##         
    ##          Miss. Clear Annie Miss. Constance Miss. Constance Gladys
    ##   female                 1               1                      1
    ##   male                   0               0                      0
    ##         
    ##          Miss. Constance Mirium Miss. Dagmar Jenny Ingeborg Miss. Daisy E
    ##   female                      1                           1             1
    ##   male                        0                           0             0
    ##         
    ##          Miss. Delia Miss. Doolina Margaret "Daisy"
    ##   female           1                              1
    ##   male             0                              0
    ##         
    ##          Miss. Dorothy Edith "Dolly" Miss. Dorothy Winifred
    ##   female                           1                      1
    ##   male                             0                      0
    ##         
    ##          Miss. Ebba Iris Alfrida Miss. Edith Corse Miss. Edith Eileen
    ##   female                       1                 1                  1
    ##   male                         0                 0                  0
    ##         
    ##          Miss. Edith Louise Miss. Edwina Celia "Winnie"
    ##   female                  1                           1
    ##   male                    0                           0
    ##         
    ##          Miss. Eleanor Ileen Miss. Eliina Miss. Elin Ester Maria
    ##   female                   1            1                      1
    ##   male                     0            0                      0
    ##         
    ##          Miss. Elina Miss. Elisabeth Walton Miss. Elise Miss. Elizabeth
    ##   female           1                      1           1               3
    ##   male             0                      0           0               0
    ##         
    ##          Miss. Elizabeth Gladys Millvina"" Miss. Elizabeth Margaret
    ##   female                                 1                        1
    ##   male                                   0                        0
    ##         
    ##          Miss. Elizabeth Mussey Miss. Elizabeth W Miss. Ellen
    ##   female                      1                 1           2
    ##   male                        0                 0           0
    ##         
    ##          Miss. Ellen "Nellie" Miss. Ellen Natalia Miss. Ellen Nellie""
    ##   female                    2                   2                    1
    ##   male                      0                   0                    0
    ##         
    ##          Miss. Ellis Anna Maria Miss. Elsie Miss. Elsie Edith Miss. Emilie
    ##   female                      1           1                 1            1
    ##   male                        0           0                 0            0
    ##         
    ##          Miss. Emily Miss. Emily Borie Miss. Emily Louisa
    ##   female           1                 1                  1
    ##   male             0                 0                  0
    ##         
    ##          Miss. Erna Alexandra Miss. Ethel Miss. Ethel Flora Miss. Eugenie
    ##   female                    1           1                 1             1
    ##   male                      0           0                 0             0
    ##         
    ##          Miss. Eva Miriam Miss. Florentina Miss. Georgette Alexandra
    ##   female                1                1                         1
    ##   male                  0                0                         0
    ##         
    ##          Miss. Gerda Ulrika Miss. Gertrud Emilia Miss. Gladys
    ##   female                  1                    1            1
    ##   male                    0                    0            0
    ##         
    ##          Miss. Grace Scott Miss. Gretchen Fiske Miss. Hannah
    ##   female                 1                    1            1
    ##   male                   0                    0            0
    ##         
    ##          Miss. Hanora "Nora" Miss. Hanora "Norah" Miss. Harriet R
    ##   female                   2                    1               1
    ##   male                     0                    0               0
    ##         
    ##          Miss. Hedwig Margaritha Miss. Helen "Ellen" Miss. Helen Alice
    ##   female                       1                   1                 1
    ##   male                         0                   0                 0
    ##         
    ##          Miss. Helen Loraine Miss. Helen Mary "Ellie" Miss. Helen Monypeny
    ##   female                   1                        1                    1
    ##   male                     0                        0                    0
    ##         
    ##          Miss. Helene Barbara Miss. Helene Ragnhild Miss. Helmina Josefina
    ##   female                    1                     1                      1
    ##   male                      0                     0                      0
    ##         
    ##          Miss. Henriette ("Mrs Harbeck") Miss. Hilda Maria
    ##   female                               1                 1
    ##   male                                 0                 0
    ##         
    ##          Miss. Hilda Mary Miss. Hildur E Miss. Hileni Miss. Honora
    ##   female                1              1            1            1
    ##   male                  0              0            0            0
    ##         
    ##          Miss. Hulda Amanda Adolfina Miss. Ida Miss. Ida Augusta Margareta
    ##   female                           1         1                           1
    ##   male                             0         0                           0
    ##         
    ##          Miss. Ida Livija Miss. Ida Sofia Miss. Ingeborg Constanzia
    ##   female                1               1                         1
    ##   male                  0               0                         0
    ##         
    ##          Miss. Jamila Miss. Jean Gertrude Miss. Jeannie Miss. Jelka
    ##   female            1                   1             2           1
    ##   male              0                   0             0           0
    ##         
    ##          Miss. Jennie Miss. Jenny Lovisa Miss. Jessie Allis
    ##   female            1                  1                  1
    ##   male              0                  0                  0
    ##         
    ##          Miss. Jessie Wills Miss. Joan Miss. Johanna Hannah"" Miss. Julia
    ##   female                  1          1                      1           2
    ##   male                    0          0                      0           0
    ##         
    ##          Miss. Julie Rachel Miss. Karen Marie Miss. Kate
    ##   female                  1                 1          5
    ##   male                    0                 0          0
    ##         
    ##          Miss. Kate Florence ("Mrs Kate Louise Phillips Marshall")
    ##   female                                                         1
    ##   male                                                           0
    ##         
    ##          Miss. Katherine Miss. Katherine "Kate" Miss. Katherine "Katie"
    ##   female               2                      1                       2
    ##   male                 0                      0                       0
    ##         
    ##          Miss. Katie Miss. Katriina Miss. Kornelia Theodosia
    ##   female           1              1                        1
    ##   male             0              0                        0
    ##         
    ##          Miss. Kristina Sofia Miss. Laina Miss. Laura Alice
    ##   female                    1           1                 1
    ##   male                      0           0                 0
    ##         
    ##          Miss. Laura Mabel Miss. Lilian W Miss. Lillian Amy
    ##   female                 1              1                 1
    ##   male                   0              0                 0
    ##         
    ##          Miss. Lillian Gertrud Miss. Louise Miss. Lucile Polk Miss. Lucy
    ##   female                     1            1                 1          1
    ##   male                       0            0                 0          0
    ##         
    ##          Miss. Luise Gretchen Miss. Lyyli Karoliina Miss. Mabel
    ##   female                    1                     1           1
    ##   male                      0                     0           0
    ##         
    ##          Miss. Mabel Helen Miss. Madeleine Miss. Madeleine Violet
    ##   female                 1               1                      1
    ##   male                   0               0                      0
    ##         
    ##          Miss. Malake Miss. Manca Miss. Manda Miss. Manta Josefina
    ##   female            1           1           1                    1
    ##   male              0           0           0                    0
    ##         
    ##          Miss. Margaret Miss. Margaret "Maggie" Miss. Margaret Bechstein
    ##   female              1                       1                        1
    ##   male                0                       0                        0
    ##         
    ##          Miss. Margaret Delia Miss. Margaret Edith Miss. Margaret Jane
    ##   female                    1                    1                   1
    ##   male                      0                    0                   0
    ##         
    ##          Miss. Margaret Marcella Maggie"" Miss. Margareth
    ##   female                                1               1
    ##   male                                  0               0
    ##         
    ##          Miss. Margit Elizabeth Miss. Marguerite Rut Miss. Mari Aina
    ##   female                      1                    1               1
    ##   male                        0                    0               0
    ##         
    ##          Miss. Maria Miss. Maria ("Mary") Miss. Maria Youssef
    ##   female           1                    1                   1
    ##   male             0                    0                   0
    ##         
    ##          Miss. Marie Catherine Miss. Marie Grice Miss. Marija Miss. Marion
    ##   female                     1                 1            2            1
    ##   male                       0                 0            0            0
    ##         
    ##          Miss. Marion Elsie Miss. Marion Louise Miss. Marjorie
    ##   female                  1                   1              1
    ##   male                    0                   0              0
    ##         
    ##          Miss. Marjorie "Lottie" Miss. Marta Miss. Mary Miss. Mary Agatha
    ##   female                       1           1          7                 1
    ##   male                         0           0          0                 0
    ##         
    ##          Miss. Mary Conover Miss. Mary Delia Miss. Mary Natalie
    ##   female                  1                1                  1
    ##   male                    0                0                  0
    ##         
    ##          Miss. Mathilde Miss. Matilda Miss. Maude Miss. May Elizabeth
    ##   female              1             1           1                   1
    ##   male                0             0           0                   0
    ##         
    ##          Miss. Nellie Miss. Nora Miss. Nora A Miss. Nourelain
    ##   female            1          1            1               1
    ##   male              0          0            0               0
    ##         
    ##          Miss. Olga Elida Miss. Phyllis May Miss. Pieta Sofia
    ##   female                1                 1                 1
    ##   male                  0                 0                 0
    ##         
    ##          Miss. Roberta Miss. Robina Maggie "Ruby" Miss. Rosalie Miss. Ruth
    ##   female             1                          1             1          1
    ##   male               0                          0             0          0
    ##         
    ##          Miss. Ruth Elizabeth Miss. Saiide Miss. Salli Helena
    ##   female                    1            1                  1
    ##   male                      0            0                  0
    ##         
    ##          Miss. Sara Rebecca Miss. Sarah Miss. Sarah A
    ##   female                  1           1             1
    ##   male                    0           0             0
    ##         
    ##          Miss. Sigrid Elisabeth Miss. Simonne Marie Anne Andree
    ##   female                      1                               1
    ##   male                        0                               0
    ##         
    ##          Miss. Stella Anna Miss. Stina Viola Miss. Susan
    ##   female                 1                 1           1
    ##   male                   0                 0           0
    ##         
    ##          Miss. Susan Parker "Suzette" Miss. Susanna Juhantytar Sanni""
    ##   female                            1                                1
    ##   male                              0                                0
    ##         
    ##          Miss. Telma Matilda Miss. Thamine Miss. Torborg Danira
    ##   female                   1             1                    1
    ##   male                     0             0                    0
    ##         
    ##          Miss. Treasteall Miss. Velin Miss. Victorine Miss. Virginia Ethel
    ##   female                1           1               1                    1
    ##   male                  0           0               0                    0
    ##         
    ##          Miss. Wendla Maria Miss. Winifred Vera
    ##   female                  1                   1
    ##   male                    0                   0
    ##         
    ##          Mlle. Berthe Antonine ("Mrs de Villiers") Mlle. Emma
    ##   female                                         1          1
    ##   male                                           0          0
    ##         
    ##          Mme. Leontine Pauline Mr. Aaron (Abi Weller")" Mr. Abraham
    ##   female                     1                        0           0
    ##   male                       0                        1           1
    ##         
    ##          Mr. Abraham (David Lishin) Mr. Abraham August Johannes
    ##   female                          0                           0
    ##   male                            1                           1
    ##         
    ##          Mr. Abraham L Mr. Achille Mr. Adola Mr. Adolf Fredrik
    ##   female             0           0         0                 0
    ##   male               1           1         1                 1
    ##         
    ##          Mr. Adolf Mathias Nicolai Olsen Mr. Adolphe Mr. Ahmed Mr. Albert
    ##   female                               0           0         0          0
    ##   male                                 1           1         1          3
    ##         
    ##          Mr. Albert A Mr. Albert Adrian Mr. Albert Francis
    ##   female            0                 0                  0
    ##   male              1                 1                  1
    ##         
    ##          Mr. Albert Johan Mr. Albert Karvin Mr. Alexander Mr. Alexander A
    ##   female                0                 0             0               0
    ##   male                  1                 1             2               1
    ##         
    ##          Mr. Alexander Morrison Mr. Alexander Oskar
    ##   female                      0                   0
    ##   male                        1                   1
    ##         
    ##          Mr. Alexander Taylor Jr Mr. Alfons Mr. Alfonzo Mr. Alfred
    ##   female                       0          0           0          0
    ##   male                         1          1           1          2
    ##         
    ##          Mr. Alfred (Baron von Drachstedt")" Mr. Alfred Fernand
    ##   female                                   0                  0
    ##   male                                     1                  1
    ##         
    ##          Mr. Alfred Fleming Mr. Alfred G Mr. Alfred George John
    ##   female                  0            0                      0
    ##   male                    1            1                      1
    ##         
    ##          Mr. Alfred J Mr. Alfred John Mr. Alfred Ossian
    ##   female            0               0                 0
    ##   male              1               1                 1
    ##         
    ##          Mr. Algernon Henry Wilson Mr. Ali Mr. Ambrose Jr Mr. Amin
    ##   female                         0       0              0        0
    ##   male                           1       2              1        1
    ##         
    ##          Mr. Anders Johan Mr. Anders Vilhelm Mr. Andrew "Andy"
    ##   female                0                  0                 0
    ##   male                  1                  1                 1
    ##         
    ##          Mr. Andrew G Mr. Anthony Mr. Anthony Wood "Archie" Mr. Anton
    ##   female            0           0                         0         0
    ##   male              1           1                         1         1
    ##         
    ##          Mr. Antoni Mr. Antti Gustaf Mr. Antti Wilhelm Mr. Apostolos
    ##   female          0                0                 0             0
    ##   male            1                1                 1             1
    ##         
    ##          Mr. Arne Jonas Mr. Arthur Mr. Arthur Ernest Mr. Arthur Gordon
    ##   female              0          0                 0                 0
    ##   male                1          2                 1                 1
    ##         
    ##          Mr. Arthur H Mr. Arthur Larned Mr. Arthur Webster Mr. Assad
    ##   female            0                 0                  0         0
    ##   male              1                 1                  1         1
    ##         
    ##          Mr. August Mr. August Edvard ("Wennerstrom") Mr. August Ferdinand
    ##   female          0                                 0                    0
    ##   male            2                                 1                    1
    ##         
    ##          Mr. August Sigfrid Mr. August Viktor Mr. Austen Mr. Austin Blyler
    ##   female                  0                 0          0                 0
    ##   male                    1                 1          1                 1
    ##         
    ##          Mr. Bartol Mr. Bengt Edvin Mr. Benjamin Mr. Benjamin Laventall
    ##   female          0               0            0                      0
    ##   male            1               1            3                      1
    ##         
    ##          Mr. Berk (Berk Trembisky) Mr. Bernard Mr. Bernt Mr. Bertram Frank
    ##   female                         0           0         0                 0
    ##   male                           1           1         1                 1
    ##         
    ##          Mr. Betros Mr. Branko Mr. Camille Mr. Carl Mr. Carl Olof
    ##   female          0          0           0        0             0
    ##   male            1          1           1        1             1
    ##         
    ##          Mr. Carl Oscar Vilhelm Gustafsson Mr. Carl Robert
    ##   female                                 0               0
    ##   male                                   1               1
    ##         
    ##          Mr. Carl/Charles Peter Mr. Cerin Mr. Chang Mr. Charles
    ##   female                      0         0         0           0
    ##   male                        1         1         1           1
    ##         
    ##          Mr. Charles (Charles Fardon) Mr. Charles Alexander
    ##   female                            0                     0
    ##   male                              1                     2
    ##         
    ##          Mr. Charles Augustus Mr. Charles Cresson Mr. Charles Duane
    ##   female                    0                   0                 0
    ##   male                      1                   1                 1
    ##         
    ##          Mr. Charles Edward Mr. Charles Emil Henry Mr. Charles Eugene
    ##   female                  0                      0                  0
    ##   male                    1                      1                  1
    ##         
    ##          Mr. Charles Frederick Mr. Charles Frederick Waddington
    ##   female                     0                                0
    ##   male                       1                                1
    ##         
    ##          Mr. Charles H Mr. Charles Hallace ("Mr C Rolmane")
    ##   female             0                                    0
    ##   male               2                                    1
    ##         
    ##          Mr. Charles Henry Mr. Charles Joseph Mr. Charles Melville
    ##   female                 0                  0                    0
    ##   male                   2                  1                    1
    ##         
    ##          Mr. Charles P Mr. Charles Robert Mr. Charles Valentine
    ##   female             0                  0                     0
    ##   male               1                  1                     1
    ##         
    ##          Mr. Charles William Mr. Choong Mr. Christo Mr. Christopher
    ##   female                   0          0           0               0
    ##   male                     1          1           1               1
    ##         
    ##          Mr. Clarence Bloomfield Mr. Claus Peter Mr. Clifford Richard
    ##   female                       0               0                    0
    ##   male                         1               1                    1
    ##         
    ##          Mr. Clifford Thomas Mr. Daniel Mr. Daniel Danielsen Mr. Daniel J
    ##   female                   0          0                    0            0
    ##   male                     1          3                    1            1
    ##         
    ##          Mr. Daniel Warner Mr. David Mr. David John Mr. David John "Dai"
    ##   female                 0         0              0                    0
    ##   male                   1         3              1                    1
    ##         
    ##          Mr. Demetrios Mr. Denis Mr. Dibo Mr. Dickinson H
    ##   female             0         0        0               0
    ##   male               1         1        1               1
    ##         
    ##          Mr. Domingos Fernandeo Mr. Douglas Bullen Mr. Eberhard Thelander
    ##   female                      0                  0                      0
    ##   male                        1                  1                      1
    ##         
    ##          Mr. Edgar Mr. Edgar Joseph Mr. Edgardo Samuel Mr. Edvard
    ##   female         0                0                  0          0
    ##   male           1                1                  1          1
    ##         
    ##          Mr. Edvard A Mr. Edvard Bengtsson Mr. Edward Mr. Edward Arthur
    ##   female            0                    0          0                 0
    ##   male              1                    1          4                 1
    ##         
    ##          Mr. Edward Austin Mr. Edward H Mr. Edward Pennington
    ##   female                 0            0                     0
    ##   male                   1            1                     1
    ##         
    ##          Mr. Edward Pomeroy Mr. Edward Roland Mr. Edward Watson Mr. Edwin
    ##   female                  0                 0                 0         0
    ##   male                    1                 1                 1         1
    ##         
    ##          Mr. Edwin Frederick"" Mr. Edwin Nelson Jr Mr. Edwy Arthur
    ##   female                     0                   0               0
    ##   male                       1                   1               1
    ##         
    ##          Mr. Einar Mr. Einar Gervasius Mr. Eino William Mr. Eiriik
    ##   female         0                   0                0          0
    ##   male           1                   1                1          1
    ##         
    ##          Mr. Elias Mr. Eliezer Mr. Elmer Zebley Mr. Emil Mr. Emile
    ##   female         0           0                0        0         0
    ##   male           1           1                1        3         1
    ##         
    ##          Mr. Emilio Mr. Emilio Ilario Giuseppe Mr. Engelhart Cornelius
    ##   female          0                          0                       0
    ##   male            1                          1                       1
    ##         
    ##          Mr. Ennis Hastings Mr. Erik Mr. Erik Gustaf
    ##   female                  0        0               0
    ##   male                    1        1               1
    ##         
    ##          Mr. Erik Gustaf (Mr Edward Lingrey")" Mr. Ernest
    ##   female                                     0          0
    ##   male                                       1          1
    ##         
    ##          Mr. Ernest Charles Mr. Ernest James Mr. Ernest Portage
    ##   female                  0                0                  0
    ##   male                    1                1                  1
    ##         
    ##          Mr. Ernest Wilfred Mr. Ernesti Arvid Mr. Ernst Adolf
    ##   female                  0                 0               0
    ##   male                    1                 1               1
    ##         
    ##          Mr. Ernst Axel Algot Mr. Ernst Gilbert Mr. Ernst Herbert
    ##   female                    0                 0                 0
    ##   male                      1                 1                 1
    ##         
    ##          Mr. Ernst Ulrik Mr. Ernst William Mr. Ervin G Mr. Escott Robert
    ##   female               0                 0           0                 0
    ##   male                 1                 1           1                 1
    ##         
    ##          Mr. Eugene Patrick Mr. Evan Mr. Fahim ("Philip Zenni") Mr. Fang
    ##   female                  0        0                          0        0
    ##   male                    1        1                          1        1
    ##         
    ##          Mr. Fared Mr. Farred Chehab Mr. Fletcher Fellows Mr. Francesco
    ##   female         0                 0                    0             0
    ##   male           1                 1                    1             1
    ##         
    ##          Mr. Francis "Frank" Mr. Francis Davis Mr. Francis William
    ##   female                   0                 0                   0
    ##   male                     1                 1                   1
    ##         
    ##          Mr. Francisco M Mr. Frank Mr. Frank Hubert Mr. Frank John
    ##   female               0         0                0              0
    ##   male                 1         2                1              1
    ##         
    ##          Mr. Frank Manley Mr. Frank Thomas Mr. Frans Olof Mr. Franz
    ##   female                0                0              0         0
    ##   male                  1                1              1         2
    ##         
    ##          Mr. Frederic Kimber Mr. Frederic Oakley Mr. Frederick
    ##   female                   0                   0             0
    ##   male                     1                   1             3
    ##         
    ##          Mr. Frederick Charles Mr. Frederick Edward Mr. Frederick James
    ##   female                     0                    0                   0
    ##   male                       1                    1                   1
    ##         
    ##          Mr. Frederick Maxfield Mr. Frederick R Mr. Frederick William
    ##   female                      0               0                     0
    ##   male                        1               1                     2
    ##         
    ##          Mr. Fridtjof Arne Mr. George Mr. George ("George Arthur Brayton")
    ##   female                 0          0                                    0
    ##   male                   1          4                                    1
    ##         
    ##          Mr. George (Mr George Thorne")" Mr. George Achilles
    ##   female                               0                   0
    ##   male                                 1                   1
    ##         
    ##          Mr. George Alexander Lucien Mr. George B Mr. George Dennick
    ##   female                           0            0                  0
    ##   male                             1            1                  1
    ##         
    ##          Mr. George Dunton Mr. George Edward Mr. George Floyd
    ##   female                 0                 0                0
    ##   male                   1                 1                1
    ##         
    ##          Mr. George Frederick Mr. George Henry Mr. George John Jr
    ##   female                    0                0                  0
    ##   male                      1                2                  1
    ##         
    ##          Mr. George Quincy Mr. George William Mr. Gerios Mr. Gerious
    ##   female                 0                  0          0           0
    ##   male                   1                  1          1           2
    ##         
    ##          Mr. Gilbert Milligan Jr Mr. Guentcho Mr. Guillaume Joseph
    ##   female                       0            0                    0
    ##   male                         1            1                    1
    ##         
    ##          Mr. Gunnar Isidor Mr. Gurshon "Gus" Mr. Gustaf Hjalmar
    ##   female                 0                 0                  0
    ##   male                   1                 1                  1
    ##         
    ##          Mr. Gustaf Joel Mr. Gustave J Mr. Hammad Mr. Hanna Mr. Hanna Assi
    ##   female               0             0          0         0              0
    ##   male                 1             1          1         3              1
    ##         
    ##          Mr. Hans Kristensen Mr. Hans Linus Mr. Hans Martin Monsen
    ##   female                   0              0                      0
    ##   male                     1              1                      1
    ##         
    ##          Mr. Hans Peder Mr. Harold J Mr. Harry Mr. Harry ("Mr E Haven")
    ##   female              0            0         0                        0
    ##   male                1            1         5                        1
    ##         
    ##          Mr. Harry Elkins Mr. Harry Markland Mr. Harvey Mr. Henrik Juul
    ##   female                0                  0          0               0
    ##   male                  1                  1          1               1
    ##         
    ##          Mr. Henry Mr. Henry Birkhardt Mr. Henry Damsgaard
    ##   female         0                   0                   0
    ##   male           2                   1                   1
    ##         
    ##          Mr. Henry Forbes Mr. Henry Harry"" Mr. Henry James Mr. Henry John
    ##   female                0                 0               0              0
    ##   male                  1                 1               1              1
    ##         
    ##          Mr. Henry Jr Mr. Henry Margido Mr. Henry Michael Mr. Henry Price
    ##   female            0                 0                 0               0
    ##   male              1                 1                 1               1
    ##         
    ##          Mr. Henry Samuel ("Mr Henry Marshall") Mr. Henry Sleeper
    ##   female                                      0                 0
    ##   male                                        1                 1
    ##         
    ##          Mr. Herbert Mr. Herbert Fuller Mr. Herbert Henry Mr. Herman
    ##   female           0                  0                 0          0
    ##   male             1                  1                 1          1
    ##         
    ##          Mr. Houssein G N Mr. Howard Brown Mr. Howard Hugh "Harry"
    ##   female                0                0                       0
    ##   male                  1                1                       1
    ##         
    ##          Mr. Hudson Joshua Creighton Mr. Hugh Mr. Hugh Roscoe Mr. Husein
    ##   female                           0        0               0          0
    ##   male                             1        1               1          1
    ##         
    ##          Mr. Ignjac Mr. Iisakki Antino Aijo Mr. Ilia Mr. Ilmari Rudolf
    ##   female          0                       0        0                 0
    ##   male            1                       1        1                 1
    ##         
    ##          Mr. Ingvald Olai Olsen Mr. Ingvar Mr. Isaac Gerald Mr. Isidor
    ##   female                      0          0                0          0
    ##   male                        1          1                1          1
    ##         
    ##          Mr. Israel Mr. Ivan Mr. Jaako Arnold Mr. Jacob
    ##   female          0        0                0         0
    ##   male            1        6                1         1
    ##         
    ##          Mr. Jacob Christian Mr. Jacques Heath Mr. Jakob Mr. Jakob Alfred
    ##   female                   0                 0         0                0
    ##   male                     1                 1         2                2
    ##         
    ##          Mr. James Mr. James Clinch Mr. James George Mr. James H
    ##   female         0                0                0           0
    ##   male           9                1                1           1
    ##         
    ##          Mr. James Matthew Mr. James Robert Mr. James Vivian
    ##   female                 0                0                0
    ##   male                   1                1                1
    ##         
    ##          Mr. Jan Baptist Mr. Janko Mr. Jean Baptiste Mr. Jean Nassr
    ##   female               0         0                 0              0
    ##   male                 1         1                 1              1
    ##         
    ##          Mr. Jego Grga Mr. Jeremiah Mr. Jeso Mr. Johan Mr. Johan Birger
    ##   female             0            0        0         0                0
    ##   male               1            1        1         2                1
    ##         
    ##          Mr. Johan Cervin Mr. Johan Charles Mr. Johan Emil
    ##   female                0                 0              0
    ##   male                  1                 1              1
    ##         
    ##          Mr. Johan Hansen Mr. Johan Henrik Johannesson Mr. Johan Julian
    ##   female                0                            0                0
    ##   male                  1                            1                1
    ##         
    ##          Mr. Johan Martin Mr. Johan Samuel Mr. Johan Svensson
    ##   female                0                0                  0
    ##   male                  1                1                  1
    ##         
    ##          Mr. Johan Werner Mr. Johann Mr. Johannes Halvorsen
    ##   female                0          0                      0
    ##   male                  1          1                      1
    ##         
    ##          Mr. Johannes Joseph Mr. John Mr. John Bertram Mr. John Borland
    ##   female                   0        0                0                0
    ##   male                     1       13                2                1
    ##         
    ##          Mr. John Borland Jr Mr. John Bradley Mr. John D Mr. John Denzil
    ##   female                   0                0          0               0
    ##   male                     1                1          1               1
    ##         
    ##          Mr. John Edward Mr. John Fredrik Alexander Mr. John George
    ##   female               0                          0               0
    ##   male                 1                          1               1
    ##         
    ##          Mr. John Hall ("Henry") Mr. John Hatfield Mr. John Henry
    ##   female                       0                 0              0
    ##   male                         1                 1              2
    ##         
    ##          Mr. John Hugo Mr. John Irwin ("Irving") Mr. John James
    ##   female             0                         0              0
    ##   male               1                         1              2
    ##         
    ##          Mr. John Joseph Mr. John Montgomery Mr. John Pillsbury
    ##   female               0                   0                  0
    ##   male                 1                   1                  1
    ##         
    ##          Mr. John Samuel Mr. John Viktor Mr. John William Mr. Jose Joaquim
    ##   female               0               0                0                0
    ##   male                 1               1                1                1
    ##         
    ##          Mr. Jose Neto Mr. Jose Pedro Mr. Josef Mr. Joseph
    ##   female             0              0         0          0
    ##   male               1              1         1          7
    ##         
    ##          Mr. Joseph Bruce Mr. Joseph Charles Mr. Joseph Holland
    ##   female                0                  0                  0
    ##   male                  1                  2                  1
    ##         
    ##          Mr. Joseph J Mr. Joseph Jr Mr. Joseph Philippe Lemercier
    ##   female            0             0                             0
    ##   male              1             1                             1
    ##         
    ##          Mr. Jovan Mr. Jovo Mr. Jozef Mr. Juha Mr. Juho Mr. Julian
    ##   female         0        0         0        0        0          0
    ##   male           1        1         1        1        2          1
    ##         
    ##          Mr. Julius Mr. Julius Konrad Eugen Mr. Kalle Edvard Mr. Kanio
    ##   female          0                       0                0         0
    ##   male            2                       1                1         1
    ##         
    ##          Mr. Karl Albert Mr. Karl Alfred Mr. Karl Edwart Mr. Karl Gideon
    ##   female               0               0               0               0
    ##   male                 1               1               1               1
    ##         
    ##          Mr. Karl Howell Mr. Karl Ivar Sven Mr. Karl Johan Mr. Karl Rudolf
    ##   female               0                  0              0               0
    ##   male                 1                  1              3               1
    ##         
    ##          Mr. Karl Siegwart Andreas Mr. Khalil Mr. Klas Albin
    ##   female                         0          0              0
    ##   male                           1          1              1
    ##         
    ##          Mr. Knud Paust Mr. Konrad Mathias Reiersen Mr. Kristo
    ##   female              0                           0          0
    ##   male                1                           1          1
    ##         
    ##          Mr. Kurt Arnold Gottfrid Mr. Lalio Mr. Lawrence Mr. Lazar Mr. Lee
    ##   female                        0         0            0         0       0
    ##   male                          1         2            2         1       2
    ##         
    ##          Mr. Len Mr. Leo Mr. Leo Edmondus Mr. Leo Peter Mr. Leon
    ##   female       0       0                0             0        0
    ##   male         1       1                1             1        1
    ##         
    ##          Mr. Leonard Charles Mr. Leonard Mark Mr. Leopold Mr. Leslie
    ##   female                   0                0           0          0
    ##   male                     1                1           1          1
    ##         
    ##          Mr. Lewis Mr. Lewis Richard Mr. Ling Mr. Linhart Mr. Lionel
    ##   female         0                 0        0           0          0
    ##   male           1                 1        1           1          1
    ##         
    ##          Mr. Liudevit Mr. Lucien Philip Mr. Luigi Mr. Luka
    ##   female            0                 0         0        0
    ##   male              1                 1         1        2
    ##         
    ##          Mr. Malkolm Joackim Mr. Mansouer Mr. Mansour Mr. Manuel Estanslas
    ##   female                   0            0           0                    0
    ##   male                     1            1           1                    1
    ##         
    ##          Mr. Mapriededer Mr. Marin Mr. Marinko Mr. Marius Mr. Mark
    ##   female               0         0           0          0        0
    ##   male                 1         1           1          1        1
    ##         
    ##          Mr. Martin Mr. Masabumi Mr. Mate Mr. Matthew Mr. Matti
    ##   female          0            0        0           0         0
    ##   male            4            1        1           1         1
    ##         
    ##          Mr. Matti Alexanteri Mr. Maurice Mr. Mauritz Hakan
    ##   female                    0           0                 0
    ##   male                      1           2                 1
    ##         
    ##          Mr. Mauritz Nils Martin Mr. Maxmillian Mr. Michael
    ##   female                       0              0           0
    ##   male                         1              1           3
    ##         
    ##          Mr. Michel ("Louis M Hoffman") Mr. Milan Mr. Mile
    ##   female                              0         0        0
    ##   male                                1         1        1
    ##         
    ##          Mr. Milton Clyde Mr. Minko Mr. Mirko Mr. Mito Mr. Mitto
    ##   female                0         0         0        0         0
    ##   male                  1         2         1        1         1
    ##         
    ##          Mr. Mohamed Mr. Moses Aaron Mr. Mustafa Mr. Nakli
    ##   female           0               0           0         0
    ##   male             1               1           1         1
    ##         
    ##          Mr. Nassef Cassem Mr. Nathan Mr. Neal Mr. Nedelio Mr. Neshan
    ##   female                 0          0        0           0          0
    ##   male                   1          1        1           1          1
    ##         
    ##          Mr. Nestor Cyriel Mr. Nicholas Mr. Nicola Mr. Niels Peder
    ##   female                 0            0          0               0
    ##   male                   1            1          1               1
    ##         
    ##          Mr. Nikola Mr. Nikolai Erland Mr. Nikolai Johannes Mr. Nils
    ##   female          0                  0                    0        0
    ##   male            1                  1                    1        1
    ##         
    ##          Mr. Nils August Mr. Nils Hilding Mr. Nils Johan Goransson
    ##   female               0                0                        0
    ##   male                 1                1                        1
    ##         
    ##          Mr. Nils Martin Mr. Noel Mr. Norman Campbell Mr. Olaf
    ##   female               0        0                   0        0
    ##   male                 1        1                   1        1
    ##         
    ##          Mr. Olaf Elon Mr. Olaus Jorgensen Mr. Ole Martin Mr. Olof
    ##   female             0                   0              0        0
    ##   male               1                   1              1        1
    ##         
    ##          Mr. Olof Edvin Mr. Orsen Mr. Ortin Mr. Oscar Wilhelm
    ##   female              0         0         0                 0
    ##   male                1         1         1                 1
    ##         
    ##          Mr. Oskar Arvid Mr. Oskar Leander Mr. Owen George Mr. Owen Harris
    ##   female               0                 0               0               0
    ##   male                 1                 1               1               1
    ##         
    ##          Mr. Pastcho ("Pentcho") Mr. Patrick Mr. Patrick D Mr. Paul Edvin
    ##   female                       0           0             0              0
    ##   male                         1          10             1              1
    ##         
    ##          Mr. Paul Romaine Mr. Pehr Fabian Oliver Malkolm Mr. Peju
    ##   female                0                              0        0
    ##   male                  1                              1        1
    ##         
    ##          Mr. Pekka Pietari Mr. Penko Mr. Percival Mr. Percival James R
    ##   female                 0         0            0                    0
    ##   male                   1         1            1                    1
    ##         
    ##          Mr. Percival Wayland Mr. Percy Andrew Mr. Percy Thomas
    ##   female                    0                0                0
    ##   male                      1                1                1
    ##         
    ##          Mr. Percy William Mr. Petar Mr. Petco
    ##   female                 0         0         0
    ##   male                   1         1         1
    ##         
    ##          Mr. Peter Andreas Lauritz Andersen Mr. Peter David
    ##   female                                  0               0
    ##   male                                    1               1
    ##         
    ##          Mr. Peter Denis Mr. Peter Henry Mr. Peter L Mr. Philemon
    ##   female               0               0           0            0
    ##   male                 1               1           1            1
    ##         
    ##          Mr. Philip Mr. Philip Joseph Mr. Philipp Edmund Mr. Phillippe
    ##   female          0                 0                  0             0
    ##   male            1                 1                  1             1
    ##         
    ##          Mr. Pierre Mr. Quigg Edmond Mr. Raffull Mr. Rahamin Haim
    ##   female          0                0           0                0
    ##   male            1                1           1                1
    ##         
    ##          Mr. Raihed Mr. Ralph Mr. Ramon Mr. Redjo Mr. Reginald
    ##   female          0         0         0         0            0
    ##   male            1         1         1         1            1
    ##         
    ##          Mr. Reginald Charles Mr. Reginald Fenton Mr. Reginald Harry
    ##   female                    0                   0                  0
    ##   male                      1                   1                  1
    ##         
    ##          Mr. Rene Mr. Rene Aime Mr. Rene Jacques Mr. Richard
    ##   female        0             0                0           0
    ##   male          1             1                1           2
    ##         
    ##          Mr. Richard Cater Mr. Richard Frasar Mr. Richard George
    ##   female                 0                  0                  0
    ##   male                   1                  1                  1
    ##         
    ##          Mr. Richard Henry Mr. Richard James Mr. Richard Leonard
    ##   female                 0                 0                   0
    ##   male                   1                 1                   1
    ##         
    ##          Mr. Richard Norris II Mr. Richard William Mr. Ristiu Mr. Robert
    ##   female                     0                   0          0          0
    ##   male                       1                   1          1          2
    ##         
    ##          Mr. Robert Douglas Mr. Robert J Mr. Robert William Norman
    ##   female                  0            0                         0
    ##   male                    1            1                         1
    ##         
    ##          Mr. Robert Williams Mr. Roderick Robert Crispin Mr. Roger
    ##   female                   0                           0         0
    ##   male                     1                           1         1
    ##         
    ##          Mr. Rossmore Edward Mr. Sahid Mr. Samuel Mr. Samuel Beard
    ##   female                   0         0          0                0
    ##   male                     1         1          5                1
    ##         
    ##          Mr. Samuel James Hayden Mr. Samuel James Metcalfe Mr. Samuel L
    ##   female                       0                         0            0
    ##   male                         1                         1            1
    ##         
    ##          Mr. Samuel Ward Mr. Sante Mr. Sarkis Mr. Satio Mr. Sebastiano
    ##   female               0         0          0         0              0
    ##   male                 1         1          2         1              1
    ##         
    ##          Mr. Selman Francis Mr. Serafino Emilio Mr. Servando Mr. Shadrach
    ##   female                  0                   0            0            0
    ##   male                    1                   1            1            1
    ##         
    ##          Mr. Shedid Mr. Sidney C Stuart Mr. Sidney Samuel
    ##   female          0                   0                 0
    ##   male            1                   1                 1
    ##         
    ##          Mr. Sigurd Hansen Mr. Simon Mr. Simon Sivertsen Mr. Sinai
    ##   female                 0         0                   0         0
    ##   male                   1         2                   1         1
    ##         
    ##          Mr. Sleiman Mr. Solomon Mr. Spencer Victor Mr. Stanio Mr. Stanko
    ##   female           0           0                  0          0          0
    ##   male             1           1                  1          1          1
    ##         
    ##          Mr. Stanley George Mr. Stanley Hubert Mr. Stefo Mr. Stephen
    ##   female                  0                  0         0           0
    ##   male                    1                  1         1           1
    ##         
    ##          Mr. Stephen Curnow Mr. Stephen Weart Mr. Stjepan Mr. Stoytcho
    ##   female                  0                 0           0            0
    ##   male                    1                 1           1            1
    ##         
    ##          Mr. Svend Lauritz Mr. Tannous Mr. Theodor Mr. Theodore Mr. Thomas
    ##   female                 0           0           0            0          0
    ##   male                   1           4           1            1          3
    ##         
    ##          Mr. Thomas Charles Mr. Thomas Clinton Mr. Thomas Drake Martinez
    ##   female                  0                  0                         0
    ##   male                    1                  1                         1
    ##         
    ##          Mr. Thomas Francis Mr. Thomas Henry Mr. Thomas J Mr. Thomas James
    ##   female                  0                0            0                0
    ##   male                    2                2            1                1
    ##         
    ##          Mr. Thomas Joseph Mr. Thomas Jr Mr. Thomas Leonard
    ##   female                 0             0                  0
    ##   male                   1             1                  1
    ##         
    ##          Mr. Thomas Parham Mr. Thomas Rowan Mr. Thomas William Solomon
    ##   female                 0                0                          0
    ##   male                   1                1                          1
    ##         
    ##          Mr. Thomson Mr. Thor Anderson Mr. Thornton Mr. Thure Edvin
    ##   female           0                 0            0               0
    ##   male             1                 1            1               1
    ##         
    ##          Mr. Tido Mr. Timothy Mr. Timothy J Mr. Todor Mr. Tome
    ##   female        0           0             0         0        0
    ##   male          1           1             1         1        1
    ##         
    ##          Mr. Tyrell William Mr. Uscher Mr. Valtcho Mr. Vasil
    ##   female                  0          0           0         0
    ##   male                    1          1           1         1
    ##         
    ##          Mr. Vassilios (Catavelas Vassilios")" Mr. Victor
    ##   female                                     0          0
    ##   male                                       1          3
    ##         
    ##          Mr. Victor de Satode Mr. Victor Francis Mr. Viktor Richard
    ##   female                    0                  0                  0
    ##   male                      1                  1                  1
    ##         
    ##          Mr. Vincenz Mr. Vivian Ponsonby Mr. Walter Mr. Walter Chamberlain
    ##   female           0                   0          0                      0
    ##   male             1                   1          1                      1
    ##         
    ##          Mr. Walter Donald Mr. Walter James Mr. Walter Miller
    ##   female                 0                0                 0
    ##   male                   1                1                 1
    ##         
    ##          Mr. Washington Augustus II Mr. Wazli Mr. Wilhelm Mr. William
    ##   female                          0         0           0           0
    ##   male                            1         1           1          10
    ##         
    ##          Mr. William A Mr. William Alfred Mr. William Anderson
    ##   female             0                  0                    0
    ##   male               1                  1                    1
    ##         
    ##          Mr. William Arthur Mr. William Augustus Mr. William Baird
    ##   female                  0                    0                 0
    ##   male                    1                    1                 1
    ##         
    ##          Mr. William Bertram Mr. William Cahoone Jr Mr. William Crothers
    ##   female                   0                      0                    0
    ##   male                     1                      1                    1
    ##         
    ##          Mr. William Edward Mr. William Ernest Mr. William Fisher
    ##   female                  0                  0                  0
    ##   male                    1                  1                  1
    ##         
    ##          Mr. William H Mr. William Henry Mr. William Henry Marsh
    ##   female             0                 0                       0
    ##   male               1                 5                       1
    ##         
    ##          Mr. William Hull Mr. William James Mr. William Jeffery
    ##   female                0                 0                   0
    ##   male                  1                 2                   1
    ##         
    ##          Mr. William John Mr. William John Robert Mr. William Neal
    ##   female                0                       0                0
    ##   male                  4                       1                1
    ##         
    ##          Mr. William Thomas Mr. William Thompson Mr. Woolf Mr. Wyckoff
    ##   female                  0                    0         0           0
    ##   male                    2                    1         1           1
    ##         
    ##          Mr. Ylio Mr. Yoto Mr. Youssef Mr. Yousseff Mrs. (Ada E Hall)
    ##   female        0        0           0            0                 1
    ##   male          1        1           1            1                 0
    ##         
    ##          Mrs. (Alice Frances) Mrs. (Amelia Milley) Mrs. (Beila)
    ##   female                    1                    1            1
    ##   male                      0                    0            0
    ##         
    ##          Mrs. (Catherine David) Mrs. (Edith Martha Bowerman)
    ##   female                      1                            1
    ##   male                        0                            0
    ##         
    ##          Mrs. (Elizabeth Anne Maidment) Mrs. (Elizabeth Ramell)
    ##   female                              1                       1
    ##   male                                0                       0
    ##         
    ##          Mrs. (Hedwig) Mrs. (Hulda Kristina Eugenia Lofqvist)
    ##   female             1                                      1
    ##   male               0                                      0
    ##         
    ##          Mrs. (Karolina) Mrs. (Lena Jacobsen Solvang) Mrs. (Lutie Davis)
    ##   female               1                            1                  1
    ##   male                 0                            0                  0
    ##         
    ##          Mrs. (Mantoura Boulos) Mrs. (Mary D Kingcome) Mrs. (Mary)
    ##   female                      1                      1           1
    ##   male                        0                      0           0
    ##         
    ##          Mrs. (Rosa) Mrs. (Selena Rogers)
    ##   female           1                    1
    ##   male             0                    0
    ##         
    ##          Mrs. Adolf Fredrik (Anna Elisabeth Judith Andersson)
    ##   female                                                    1
    ##   male                                                      0
    ##         
    ##          Mrs. Albert (Antoinette Magnin)
    ##   female                               1
    ##   male                                 0
    ##         
    ##          Mrs. Albert Adrian (Vera Gillespie)
    ##   female                                   1
    ##   male                                     0
    ##         
    ##          Mrs. Albert Francis (Sylvia Mae Harbaugh)
    ##   female                                         1
    ##   male                                           0
    ##         
    ##          Mrs. Alexander (Helga E Lindqvist)
    ##   female                                  1
    ##   male                                    0
    ##         
    ##          Mrs. Alexander (Thamine Thelma")"
    ##   female                                 1
    ##   male                                   0
    ##         
    ##          Mrs. Alexander A (Grace Charity Laury)
    ##   female                                      1
    ##   male                                        0
    ##         
    ##          Mrs. Alexander Oskar (Mary Aline Towner)
    ##   female                                        1
    ##   male                                          0
    ##         
    ##          Mrs. Alexander Taylor (Mary Eliza Ingersoll)
    ##   female                                            1
    ##   male                                              0
    ##         
    ##          Mrs. Alfred (Antoinette) Mrs. Allen Oliver (Nellie E Baumgardner)
    ##   female                        1                                        1
    ##   male                          0                                        0
    ##         
    ##          Mrs. Amin S (Marie Marthe Thuillard)
    ##   female                                    1
    ##   male                                      0
    ##         
    ##          Mrs. Anders Johan (Alfrida Konstantia Brogren)
    ##   female                                              1
    ##   male                                                0
    ##         
    ##          Mrs. Andrew G (Elizabeth Lily" Watson)"
    ##   female                                       1
    ##   male                                         0
    ##         
    ##          Mrs. Anton (Luise Heilmann) Mrs. Antoni (Selini Alexander)
    ##   female                           1                              1
    ##   male                             0                              0
    ##         
    ##          Mrs. Arthur Henry (Addie" Dart Trevaskis)"
    ##   female                                          1
    ##   male                                            0
    ##         
    ##          Mrs. Arthur Larned (Emily Maria Borie) Mrs. Benjamin (Edith Nile)
    ##   female                                      1                          1
    ##   male                                        0                          0
    ##         
    ##          Mrs. Benjamin (Ellen Truelove Arman)
    ##   female                                    1
    ##   male                                      0
    ##         
    ##          Mrs. Benjamin (Esther Ada Bloomfield)
    ##   female                                     1
    ##   male                                       0
    ##         
    ##          Mrs. Bertram (Eva Georgetta Light)
    ##   female                                  1
    ##   male                                    0
    ##         
    ##          Mrs. Betros (Zahie Maria" Elias)" Mrs. Boulton (Olive Potter)
    ##   female                                 1                           1
    ##   male                                   0                           0
    ##         
    ##          Mrs. Carl Johan (Sigrid Posse)
    ##   female                              1
    ##   male                                0
    ##         
    ##          Mrs. Carl Oscar (Selma Augusta Emilia Johansson)
    ##   female                                                1
    ##   male                                                  0
    ##         
    ##          Mrs. Catherine (Catherine Rizk)
    ##   female                               1
    ##   male                                 0
    ##         
    ##          Mrs. Charles Alexander (Alice Adelaide Slow)
    ##   female                                            1
    ##   male                                              0
    ##         
    ##          Mrs. Charles Emil Henry (Annie May Morris)
    ##   female                                          1
    ##   male                                            0
    ##         
    ##          Mrs. Charles Melville (Clara Jennings Gregg)
    ##   female                                            1
    ##   male                                              0
    ##         
    ##          Mrs. Charles V (Ada Maria Winfield)
    ##   female                                   1
    ##   male                                     0
    ##         
    ##          Mrs. Claus Peter (Jennie L Howard)
    ##   female                                  1
    ##   male                                    0
    ##         
    ##          Mrs. Daniel Warner (Mary Graham Carmichael Farquarson)
    ##   female                                                      1
    ##   male                                                        0
    ##         
    ##          Mrs. Darwis (Hanne Youssef Razi) Mrs. Dickinson H (Helen Walton)
    ##   female                                1                               1
    ##   male                                  0                               0
    ##         
    ##          Mrs. Edgar Joseph (Leila Saks)
    ##   female                              1
    ##   male                                0
    ##         
    ##          Mrs. Edvard Bengtsson (Elin Gerda Persson)
    ##   female                                          1
    ##   male                                            0
    ##         
    ##          Mrs. Edward (Ethel Clarke)
    ##   female                          1
    ##   male                            0
    ##         
    ##          Mrs. Edward (Helen Churchill Hungerford)
    ##   female                                        1
    ##   male                                          0
    ##         
    ##          Mrs. Edward (Margaret Ann Watson)
    ##   female                                 1
    ##   male                                   0
    ##         
    ##          Mrs. Edward Dale (Charlotte Lamson)
    ##   female                                   1
    ##   male                                     0
    ##         
    ##          Mrs. Edward Gifford (Catherine Elizabeth Halstead)
    ##   female                                                  1
    ##   male                                                    0
    ##         
    ##          Mrs. Edward Scott (Elisabeth Walton McMillan)
    ##   female                                             1
    ##   male                                               0
    ##         
    ##          Mrs. Edwin Nelson Jr (Gertrude Parsons)
    ##   female                                       1
    ##   male                                         0
    ##         
    ##          Mrs. Edwy Arthur (Ada Mary Worth) Mrs. Elizabeth (Eliza Needs)
    ##   female                                 1                            1
    ##   male                                   0                            0
    ##         
    ##          Mrs. Elmer Zebley (Juliet Cummins Wright)
    ##   female                                         1
    ##   male                                           0
    ##         
    ##          Mrs. Emil (Tillie Mandelbaum)
    ##   female                             1
    ##   male                               0
    ##         
    ##          Mrs. Ernest Courtenay (Lilian Hughes)
    ##   female                                     1
    ##   male                                       0
    ##         
    ##          Mrs. Ernest H (Elizabeth Lindsey James)
    ##   female                                       1
    ##   male                                         0
    ##         
    ##          Mrs. Ernst Gilbert (Anna Sigrid Maria Brogren) Mrs. Fatima
    ##   female                                              1           1
    ##   male                                                0           0
    ##         
    ##          Mrs. Florence "Fannie" Mrs. Frank (Frances)
    ##   female                      1                    1
    ##   male                        0                    0
    ##         
    ##          Mrs. Frank John (Emily Alice Brown)
    ##   female                                   1
    ##   male                                     0
    ##         
    ##          Mrs. Frank Manley (Anna Sophia Atkinson)
    ##   female                                        1
    ##   male                                          0
    ##         
    ##          Mrs. Frederic Oakley (Margaretta Corning Stone)
    ##   female                                               1
    ##   male                                                 0
    ##         
    ##          Mrs. Frederick (Augusta Tyler)
    ##   female                              1
    ##   male                                0
    ##         
    ##          Mrs. Frederick Charles (Jane Richards)
    ##   female                                      1
    ##   male                                        0
    ##         
    ##          Mrs. Frederick Charles (Mary Helene Baxter)
    ##   female                                           1
    ##   male                                             0
    ##         
    ##          Mrs. Frederick Joel (Margaret Welles Barron)
    ##   female                                            1
    ##   male                                              0
    ##         
    ##          Mrs. Frederick Maxfield (Jane Anne Forby)
    ##   female                                         1
    ##   male                                           0
    ##         
    ##          Mrs. Frederick R (Marion) Mrs. George (Omine Amenia" Alexander)"
    ##   female                         1                                      1
    ##   male                           0                                      0
    ##         
    ##          Mrs. George Achilles (Dorothy Annan)
    ##   female                                    1
    ##   male                                      0
    ##         
    ##          Mrs. George Dennick (Mary Hitchcock)
    ##   female                                    1
    ##   male                                      0
    ##         
    ##          Mrs. George Dunton (Eleanor Elkins)
    ##   female                                   1
    ##   male                                     0
    ##         
    ##          Mrs. George Joseph (Shawneene Abi-Saab)
    ##   female                                       1
    ##   male                                         0
    ##         
    ##          Mrs. George Nelson (Martha Evelyn) Mrs. Gertrude Maybelle
    ##   female                                  1                      1
    ##   male                                    0                      0
    ##         
    ##          Mrs. Guillaume Joseph (Emma) Mrs. Harvey (Charlotte Annie Tate)
    ##   female                            1                                  1
    ##   male                              0                                  0
    ##         
    ##          Mrs. Henry Arthur Jr (Eleanor Genevieve Fosdick)
    ##   female                                                1
    ##   male                                                  0
    ##         
    ##          Mrs. Henry Birkhardt (Irene Wallach)
    ##   female                                    1
    ##   male                                      0
    ##         
    ##          Mrs. Henry Sleeper (Myna Haxtun)
    ##   female                                1
    ##   male                                  0
    ##         
    ##          Mrs. Henry William (Clara Heinsheimer)
    ##   female                                      1
    ##   male                                        0
    ##         
    ##          Mrs. Herbert Fuller (Carrie Constance Toogood)
    ##   female                                              1
    ##   male                                                0
    ##         
    ##          Mrs. Hjalmar (Agnes Charlotta Bengtsson)
    ##   female                                        1
    ##   male                                          0
    ##         
    ##          Mrs. Hudson J C (Bessie Waldo Daniels)
    ##   female                                      1
    ##   male                                        0
    ##         
    ##          Mrs. Isidor (Rosalie Ida Blun) Mrs. J Frank (Claire Bennett)
    ##   female                              1                             1
    ##   male                                0                             0
    ##         
    ##          Mrs. Jacques Heath (Lily May Peel)
    ##   female                                  1
    ##   male                                    0
    ##         
    ##          Mrs. James (Elizabeth "Bessie" Inglis Milne)
    ##   female                                            1
    ##   male                                              0
    ##         
    ##          Mrs. James (Ellen Needs) Mrs. James (Helene DeLaudeniere Chaput)
    ##   female                        1                                       1
    ##   male                          0                                       0
    ##         
    ##          Mrs. James Joseph (Margaret Tobin)
    ##   female                                  1
    ##   male                                    0
    ##         
    ##          Mrs. James Vivian (Lulu Thorne Christian)
    ##   female                                         1
    ##   male                                           0
    ##         
    ##          Mrs. James Warburton Martinez (Charlotte Wardle Drake)
    ##   female                                                      1
    ##   male                                                        0
    ##         
    ##          Mrs. Jean Baptiste (Rosalie Paula Govaert)
    ##   female                                          1
    ##   male                                            0
    ##         
    ##          Mrs. Johan (Johanna Persdotter Larsson) Mrs. John (Annie Bullen)
    ##   female                                       1                        1
    ##   male                                         0                        0
    ##         
    ##          Mrs. John (Catherine)
    ##   female                     1
    ##   male                       0
    ##         
    ##          Mrs. John Borland (Marian Longstreth Morris)
    ##   female                                            1
    ##   male                                              0
    ##         
    ##          Mrs. John Bradley (Florence Briggs Thayer)
    ##   female                                          1
    ##   male                                            0
    ##         
    ##          Mrs. John C (Anna Andrews) Mrs. John Henry (Sara Elizabeth Lawry)
    ##   female                          1                                      1
    ##   male                            0                                      0
    ##         
    ##          Mrs. John Jacob (Madeleine Talmadge Force)
    ##   female                                          1
    ##   male                                            0
    ##         
    ##          Mrs. John James (Florence Louise Long)
    ##   female                                      1
    ##   male                                        0
    ##         
    ##          Mrs. John Morgan (Elizabeth Agnes Mary White)
    ##   female                                             1
    ##   male                                               0
    ##         
    ##          Mrs. John Murray (Caroline Lane Lamson)
    ##   female                                       1
    ##   male                                         0
    ##         
    ##          Mrs. John Pillsbury (Nelle Stevenson)
    ##   female                                     1
    ##   male                                       0
    ##         
    ##          Mrs. John Stuart (Ella Holmes) Mrs. John T (Ada Julia Bone)
    ##   female                              1                            1
    ##   male                                0                            0
    ##         
    ##          Mrs. Josef (Josefine Franchi)
    ##   female                             1
    ##   male                               0
    ##         
    ##          Mrs. Joseph (Juliette Marie Louise Lafargue)
    ##   female                                            1
    ##   male                                              0
    ##         
    ##          Mrs. Joseph (Maria Elias) Mrs. Joseph (Sophie Halaut Easu)
    ##   female                         1                                1
    ##   male                           0                                0
    ##         
    ##          Mrs. Joseph (Sultana) Mrs. Juha (Maria Emilia Ojala)
    ##   female                     1                              1
    ##   male                       0                              0
    ##         
    ##          Mrs. Julius (Emelia Maria Vandemoortele)
    ##   female                                        1
    ##   male                                          0
    ##         
    ##          Mrs. Karl Alfred (Maria Mathilda Gustafsson)
    ##   female                                            1
    ##   male                                              0
    ##         
    ##          Mrs. Leo David (Blanche Strouse) Mrs. Leonard (Pauline C Boeson)
    ##   female                                1                               1
    ##   male                                  0                               0
    ##         
    ##          Mrs. Leopold (Mathilde Francoise Pede)
    ##   female                                      1
    ##   male                                        0
    ##         
    ##          Mrs. Lizzie (Elizabeth Anne Wilkinson)
    ##   female                                      1
    ##   male                                        0
    ##         
    ##          Mrs. Louis Albert (Ida Sophia Fischer)
    ##   female                                      1
    ##   male                                        0
    ##         
    ##          Mrs. Lucien Philip (Mary Eloise Hughes) Mrs. Mara
    ##   female                                       1         1
    ##   male                                         0         0
    ##         
    ##          Mrs. Mariana (Miriam")" Mrs. Mark (Mary McDougald)
    ##   female                       1                          1
    ##   male                         0                          0
    ##         
    ##          Mrs. Martin (Elizabeth L. Barrett)
    ##   female                                  1
    ##   male                                    0
    ##         
    ##          Mrs. Maxmillian (Margaretha Emerentia Stehli)
    ##   female                                             1
    ##   male                                               0
    ##         
    ##          Mrs. Neal (Eileen O'Leary) Mrs. Nicholas (Adele Achem)
    ##   female                          1                           1
    ##   male                            0                           0
    ##         
    ##          Mrs. Nils (Alma Cornelia Berglund)
    ##   female                                  1
    ##   male                                    0
    ##         
    ##          Mrs. Norman Campbell (Bertha Griggs)
    ##   female                                    1
    ##   male                                      0
    ##         
    ##          Mrs. Oscar W (Elisabeth Vilhelmina Berg) Mrs. Paul (Emma Mock)
    ##   female                                        1                     1
    ##   male                                          0                     0
    ##         
    ##          Mrs. Pekka Pietari (Elin Matilda Dolck)
    ##   female                                       1
    ##   male                                         0
    ##         
    ##          Mrs. Percival (Florence Kate White)
    ##   female                                   1
    ##   male                                     0
    ##         
    ##          Mrs. Percy C (Mary Phyllis Elizabeth Miller)
    ##   female                                            1
    ##   male                                              0
    ##         
    ##          Mrs. Peter Henry (Lillian Jefferys)
    ##   female                                   1
    ##   male                                     0
    ##         
    ##          Mrs. Richard Leonard (Sallie Monypeny)
    ##   female                                      1
    ##   male                                        0
    ##         
    ##          Mrs. Robert Clifford (Malvina Helen Lamson)
    ##   female                                           1
    ##   male                                             0
    ##         
    ##          Mrs. Said (Waika Mary" Mowad)" Mrs. Sam (Leah Rosen)
    ##   female                              1                     1
    ##   male                                0                     0
    ##         
    ##          Mrs. Samuel (Emma) Mrs. Samuel (Hannah Wizosky)
    ##   female                  1                            1
    ##   male                    0                            0
    ##         
    ##          Mrs. Samuel (Jane Laver) Mrs. Samuel L (Edwiga Grabowska)
    ##   female                        1                                1
    ##   male                          0                                0
    ##         
    ##          Mrs. Sebastiano (Argenia Genovesi) Mrs. Sidney (Emily Hocking)
    ##   female                                  1                           1
    ##   male                                    0                           0
    ##         
    ##          Mrs. Sidney Samuel (Amy Frances Christy)
    ##   female                                        1
    ##   male                                          0
    ##         
    ##          Mrs. Sinai (Miriam Sternin) Mrs. Solomon (Latifa Qurban)
    ##   female                           1                            1
    ##   male                             0                            0
    ##         
    ##          Mrs. Stanton (Rosa Hunt) Mrs. Stephen (Annie Margaret Hill)
    ##   female                        1                                  1
    ##   male                          0                                  0
    ##         
    ##          Mrs. Thomas (Annie Louise Rowley) Mrs. Thomas (Edith Wearne)
    ##   female                                 1                          1
    ##   male                                   0                          0
    ##         
    ##          Mrs. Thomas (Johanna "Hannah" Godfrey)
    ##   female                                      1
    ##   male                                        0
    ##         
    ##          Mrs. Thomas Henry (Mary E Finck)
    ##   female                                1
    ##   male                                  0
    ##         
    ##          Mrs. Thomas Jr (Lily Alexenia Wilson)
    ##   female                                     1
    ##   male                                       0
    ##         
    ##          Mrs. Thomas William Solomon (Elizabeth Catherine Ford)
    ##   female                                                      1
    ##   male                                                        0
    ##         
    ##          Mrs. Thornton (Orian Hays)
    ##   female                          1
    ##   male                            0
    ##         
    ##          Mrs. Tyrell William (Julia Florence Siegel)
    ##   female                                           1
    ##   male                                             0
    ##         
    ##          Mrs. Victor de Satode (Maria Josefa Perez de Soto y Vallejo)
    ##   female                                                            1
    ##   male                                                              0
    ##         
    ##          Mrs. Viktor (Helena Wilhelmina)
    ##   female                               1
    ##   male                                 0
    ##         
    ##          Mrs. Walter Bertram (Martha Eustis)
    ##   female                                   1
    ##   male                                     0
    ##         
    ##          Mrs. Walter Donald (Mahala Dutton) Mrs. Walter H (Irene Colvin)
    ##   female                                  1                            1
    ##   male                                    0                            0
    ##         
    ##          Mrs. Walter Miller (Virginia McDowell)
    ##   female                                      1
    ##   male                                        0
    ##         
    ##          Mrs. Washington (Ruth Vidaver)
    ##   female                              1
    ##   male                                0
    ##         
    ##          Mrs. Wilhelm (Elna Matilda Persson)
    ##   female                                   1
    ##   male                                     0
    ##         
    ##          Mrs. William (Anna Bernhardina Karlsson)
    ##   female                                        1
    ##   male                                          0
    ##         
    ##          Mrs. William (Anna Sylfven) Mrs. William (Anna)
    ##   female                           1                   1
    ##   male                             0                   0
    ##         
    ##          Mrs. William (Imanita Parrish Hall)
    ##   female                                   1
    ##   male                                     0
    ##         
    ##          Mrs. William (Margaret Norton)
    ##   female                              1
    ##   male                                0
    ##         
    ##          Mrs. William (Winnie Minnie" Treanor)"
    ##   female                                      1
    ##   male                                        0
    ##         
    ##          Mrs. William A (Florence "Mary" Agnes Hughes)
    ##   female                                             1
    ##   male                                               0
    ##         
    ##          Mrs. William Arthur (Cordelia K Stanlick)
    ##   female                                         1
    ##   male                                           0
    ##         
    ##          Mrs. William Augustus (Marie Eugenie)
    ##   female                                     1
    ##   male                                       0
    ##         
    ##          Mrs. William Baird (Alice Munger)
    ##   female                                 1
    ##   male                                   0
    ##         
    ##          Mrs. William Edward (Lillian E Thorpe)
    ##   female                                      1
    ##   male                                        0
    ##         
    ##          Mrs. William Ernest (Lucile Polk) Mrs. William H (Jessie L)
    ##   female                                 1                         1
    ##   male                                   0                         0
    ##         
    ##          Mrs. William John Robert (Dorothy Ann Wonnacott)
    ##   female                                                1
    ##   male                                                  0
    ##         
    ##          Mrs. William Robert (Emma Eliza Ward)
    ##   female                                     1
    ##   male                                       0
    ##         
    ##          Mrs. William Thompson (Edith Junkins) Ms. Bridget Ms. Encarnacion
    ##   female                                     1           1               1
    ##   male                                       0           0               0
    ##         
    ##          Rev. Charles Leonard Rev. Ernest Courtenay Rev. John
    ##   female                    0                     0         0
    ##   male                      1                     1         1
    ##         
    ##          Rev. Joseph Maria Rev. Juozas Rev. Robert James
    ##   female                 0           0                 0
    ##   male                   1           1                 1
    ##         
    ##          Rev. Thomas Roussel Davids Rev. William
    ##   female                          0            0
    ##   male                            1            1
    ##         
    ##          Sir. Cosmo Edmund ("Mr Morgan")
    ##   female                               0
    ##   male                                 1
    ##         
    ##          the Countess. of (Lucy Noel Martha Dyer-Edwards)
    ##   female                                                1
    ##   male                                                  0

Titles with very low cell counts to be combined to “rare” level in the
chunk below.

``` r
rare_title <- c("Dona", "Lady", "the Countess",
                "Capt", "Col", "Don", "Dr",
                "Major", "Rev", "Sir", "Jonkheer")
# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == "Mlle"] = "Miss"
full$Title[full$Title == "Ms"] = "Miss"
full$Title[full$Title == "Mme"] = "Mrs"
full$Title[full$Title %in% rare_title] = "rare_title"
```

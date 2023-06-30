# packages ===================================================================
library(dplyr)
library(tibble)
library(rpart)
library(rpart.plot)

# data =======================================================================
train <- read.csv("../data/train.csv") %>% tibble::tibble()
test <- read.csv("../data/test.csv") %>% tibble::tibble()

# exploratory analysis =======================================================
colnames(train)
train %>%
  dplyr::group_by(Transported) %>%
  dplyr::count(HomePlanet) %>%
  tidyr::pivot_wider(
    id_cols = "HomePlanet", 
    names_from = "Transported",
    values_from = "n"
  )
# something happened with people from Europa, most of them got transported

train %>%
  dplyr::group_by(Transported) %>%
  dplyr::count(CryoSleep) %>%
  tidyr::pivot_wider(
    id_cols = "CryoSleep", 
    names_from = "Transported",
    values_from = "n"
  )
# people sleeping in CryoSleep have more chances to be transported

train %>%
  dplyr::filter(HomePlanet == "Europa") %>%
  dplyr::group_by(Transported) %>%
  dplyr::count(CryoSleep) %>%
  tidyr::pivot_wider(
    id_cols = "CryoSleep", 
    names_from = "Transported",
    values_from = "n"
  )

train %>%
  dplyr::select(Transported, Cabin) %>%
  # dplyr::arrange(Cabin) %>%
  dplyr::mutate(
    deck = stringr::str_remove(Cabin, "\\/.*"),
    num = stringr::str_remove(Cabin, "\\/.$") %>% stringr::str_remove(".*\\/"),
    side = stringr::str_remove(Cabin, ".*/\\.*")
  ) %>%
  dplyr::group_by(Transported) %>%
  dplyr::count(side) %>%
  tidyr::pivot_wider(
    id_cols = "side", 
    names_from = "Transported",
    values_from = "n"
  )
# no conclusions

train %>%
  dplyr::group_by(Transported) %>%
  dplyr::count(Destination) %>%
  tidyr::pivot_wider(
    id_cols = "Destination", 
    names_from = "Transported",
    values_from = "n"
  )
# no conclusions

train %>%
  dplyr::group_by(Transported) %>%
  dplyr::count(Age) %>%
  tidyr::pivot_wider(
    id_cols = "Age", 
    names_from = "Transported",
    values_from = "n"
  )
# no conclusions

train %>%
  dplyr::group_by(Transported) %>%
  dplyr::count(VIP) %>%
  tidyr::pivot_wider(
    id_cols = "VIP", 
    names_from = "Transported",
    values_from = "n"
  )
# no conclusions

train %>%
  dplyr::group_by(Transported) %>%
  dplyr::count(VRDeck) %>%
  tidyr::pivot_wider(
    id_cols = "VRDeck", 
    names_from = "Transported",
    values_from = "n"
  ) %>%
  dplyr::arrange(desc(VRDeck)) %>%
  na.omit()
# using luxury amenities decreases chances of being transported

train %>%
  dplyr::select(Transported, Name, HomePlanet) %>%
  dplyr::mutate(
    first_name = purrr::map_chr(
      Name, 
      ~stringr::str_split(.x, " ", simplify = T) %>% unlist() %>% .[1]
    ),
    last_name = purrr::map_chr(
      Name, 
      ~stringr::str_split(.x, " ", simplify = T) %>% unlist() %>% .[1]
    )
  ) %>%
  dplyr::group_by(Transported) %>%
  dplyr::count(last_name, HomePlanet) %>%
  tidyr::pivot_wider(
    id_cols = c("last_name","HomePlanet"), 
    names_from = "Transported",
    values_from = "n"
  ) %>%
  arrange(HomePlanet, desc(True)) %>%
  filter(HomePlanet == "Europa")
# no conclusions


# decision tree ==============================================================
train_ok <- train %>%
  dplyr::mutate(
    Transported = factor(Transported, levels = c("True","False")),
    deck = stringr::str_remove(Cabin, "\\/.*"),
    num = stringr::str_remove(Cabin, "\\/.$") %>% stringr::str_remove(".*\\/") %>% as.numeric(),
    side = stringr::str_remove(Cabin, ".*/\\.*")
  ) %>%
  dplyr::mutate(
    luxury = RoomService + FoodCourt + ShoppingMall + Spa + VRDeck
  ) #%>%
  # dplyr::filter(CryoSleep != "")
# View(train_ok)
vars <- c("Transported", "CryoSleep","Age")
m <- rpart::rpart(Transported ~ ., data = train_ok[,vars])
rpart.plot(m)

p <- predict(m, train_ok, type = 'class')
caret::confusionMatrix(p, train_ok$Transported, positive="True")
predict(m, test, type = "prob") %>%
  as.data.frame() %>%
  tibble::tibble() %>%
  dplyr::bind_cols(test[,"PassengerId"]) %>%
  dplyr::mutate(Transported = ifelse(True > False, "True","False")) %>%
  dplyr::select(PassengerId, Transported)

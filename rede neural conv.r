# Carregar bibliotecas
library(keras)
library(tensorflow)
install_tensorflow()
install_keras()
install.packages("tensorflow")
# Carregar o dataset CIFAR-10
cifar <- dataset_cifar10()

# Dividir o dataset em dados de treino e teste
x_train <- cifar$train$x
y_train <- cifar$train$y
x_test <- cifar$test$x
y_test <- cifar$test$y

# Normalizar os dados (dividir por 255 para que fiquem entre 0 e 1)
x_train <- x_train / 255
x_test <- x_test / 255

# Converter as classes em formato one-hot encoding
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

# Construção da Rede Neural Convolucional (CNN)
model <- keras_model_sequential() %>%
  # Camada Convolucional 1
  layer_conv_2d(filters = 32, kernel_size = c(3, 3),
                activation = "relu",
                input_shape = c(32, 32, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%

  # Camada Convolucional 2
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%

  # Camada Convolucional 3
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%

  # Flatten (transformar para vetor)
  layer_flatten() %>%

  # Camada densa (Fully connected layer)
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 10, activation = "softmax")

# Compilar o modelo
model %>% compile(
  loss = "categorical_crossentropy", # Função de perda para multi-classe
  optimizer = optimizer_adam(),      # Adam optimizer
  metrics = c("accuracy")
)

# Resumo da arquitetura do modelo
summary(model)

# Treinar o modelo
history <- model %>% fit(
  x_train, y_train,
  epochs = 10,           # Número de épocas
  batch_size = 64,       # Tamanho do lote
  validation_data = list(x_test, y_test)  # Dados de validação
)

# Avaliar o modelo no conjunto de teste
score <- model %>% evaluate(x_test, y_test)
cat("Loss:", score$loss, "Accuracy:", score$accuracy)

# Fazer previsões no conjunto de teste
predictions <- model %>% predict(x_test)

# Ver as primeiras 5 previsões
head(predictions, 5)

# Projeto da disciplina MCCC015-23-Programação Funcional UFABC - 2024
## Autor: 
- Paulo Alexandre Pizará Hayashida RA: 11201722652

# Guess-Word

Este projeto é baseado no jogo Wordle e Termo onde o jogador deve adivinhar a palavra de cinco letras em seis tentativas, sendo que o jogo informa a letras que fazem parte da palavra e estão na posição certa, estão na posição errada e não fazem parte da palavra, o jogador ganha caso consiga adivinhar a palavra em seis tentativas, o jogo fornece uma palavra por dia. Nesta versão iremos utilizar palavras do português em com quantidade arbitrária de letras.

## Instruções: 
Execute stack build para compilar o projeto e em seguida stack run para executar o programa irá perguntar a quantidade de letras desejadas para a palavra secreta, entre com o valor, em seguida o jogo irá iniciar uma janela no gloss, onde a quantidade de linhas é a quantidades de tentativas que você terá para acertar a palavra secreta, e cada coluna resenta uma letra da palavra secreta. Entre com as letras que deseja, quando todas as colunas de uma linha forem preenchidas o jogo irá colorir as colunas, as colunas vermelhas indicam letras que não fazem parte da palavra, as amarelas fazem parte da palavra, mas estão na posição errada e verde estão na posição correta e fazem parte da palavra. O jogo continuar até acertar a palavra secreta ou as tentativas acabarem.

## Código: 
O código é composto por cinco arquivos principais, Main.hs, Game.hs, GameGraphics.hs , Lib.hs e Sepc.hs. Spec.hs possui os testes unitários de algumas funções que pode ser executado com stack test, Lib.hs é o arquivo que contém as funções auxiliares do projeto, Game.hs possui as funções eu executam o jogo em si, GameGraphics.hs contém funções de criar os objetos da tela do Gloss e Main.hs executa o programa chamando as funções auxiliares.



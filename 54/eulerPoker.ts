const fs = require("fs");

const s = fs.readFileSync('input.txt', 'utf8');
const lines = s.split("\n");
let gamesCounter = 0;
let player1Points = 0;
let player2Points = 0;
let matchesPlayer1Won = [];
let ties = 0;
console.log(lines[lines.length-1]);

for(let i = 0; i < lines.length-1; i++){  
  gamesCounter++;
  const cards = lines[i].split(" ");
  // console.log(cards);
  let player1 = {
    cards: [],
    suits: [],
    combination: "",
    value: 0,
    tieBreaker: 0,
    highest: 0
  }

  let player2 = {
    cards: [],
    suits: [],
    combination: "",
    value: 0,
    tieBreaker: 0,
    highest: 0
  }

  const cards1 = cards.filter((card, index) => index <= 4);
  const cards2 = cards.filter((card, index) => index > 4);
  player1.suits = cards1.map((card) => card[card.length - 1]);
  player2.suits = cards2.map((card) => card[card.length - 1]);


  const player1Numbers = cards1.map(card =>
    card[0] === "T" && (card = `10${card[1]}`) || 
    card[0] === "J" && (card = `11${card[1]}`) || 
    card[0] === "Q" && (card = `12${card[1]}`) || 
    card[0] === "K" && (card = `13${card[1]}`) || 
    card[0] === "A" && (card = `14${card[1]}`) ||
    card
    );

  const player2Numbers = cards2.map( card =>
    card[0] === "T" && (card = `10${card[1]}`) || 
    card[0] === "J" && (card = `11${card[1]}`) || 
    card[0] === "Q" && (card = `12${card[1]}`) || 
    card[0] === "K" && (card = `13${card[1]}`) || 
    card[0] === "A" && (card = `14${card[1]}`) ||
    card
    );

  player1.cards = player1Numbers.map((card) => parseInt(card.slice(0, card.length - 1))).sort((a,b) => a-b);
  player2.cards = player2Numbers.map((card) => parseInt(card.slice(0, card.length - 1))).sort((a,b) => a-b);
  player1.highest = player1.cards[4];
  player2.highest = player2.cards[4];

  const flush = function (player) {
    for(let i = 0; i <= 3; i++) {
      if (player.suits[i] !== player.suits[i+1]) {
        return 
      } 
    }
      
    royalFlush(player)}



  const royalFlush = function (player) {
    const royal = [10, 11, 12, 13, 14]
    for(let i = 0; i <= 4; i++ ){
      if (player.cards[i] !== royal[i]){
        return straightFlush(player)
      }
    }
    player.value = 10;
    player.combination = "Royal Flush";
    return 
  };

  const straight = function (player) {
    let counter = 1;
    for(let i = 0; i <= 3; i++){
      (player.cards[i] +1) === player.cards[i+1] && counter++ 
    }
    if(counter === 5){
      player.value = 5;
      player.tieBreaker = player.cards[4]; 
      player.combination = "Straight";
      return 
    }
  }

  const straightFlush = function (player) {
    if(player.value === 5){
      player.value = 9;
      player.tieBreaker = player.cards[4]; 
      player.combination = "Straight Flush";
      return 
    }
    player.value = 6;
    player.tieBreaker = player.cards[4];
    player.combination = "Flush";
    return 
    
  }

  const fullHouse = function (player) {
    let remainingCards = [];
    for(let i = 0; i <=2; i++){
      if((player.cards[i] === player.cards[i+1]) && (player.cards[i] === player.cards[i+2])) {
          remainingCards = player.cards.filter((card, index) => (index !== i) && (index !== (i+1)) && (index !== (i+2)));
          player.tieBreaker = player.cards[i];
          break;
        }
      }
    if((remainingCards.length === 2) && (remainingCards[0] === remainingCards[1])){
      player.value = 7;
      player.combination = "Full House";
      return 
    }

  }

  const fourOfKind = function (player) {
    for(let i = 0; i <= 1; i++) {
      if((player.cards[i] === player.cards[i+1]) && 
        (player.cards[i] === player.cards[i+2]) && 
        (player.cards[i] === player.cards[i+3])) 
        {
          player.value = 8;
          player.tieBreaker = player.cards[i];
          player.combination = "Four of a Kind";
          return 
  }}}

  const threeOfKind = function (player) {
    for(let i = 0; i <=2; i++){
      if((player.cards[i] === player.cards[i+1]) && (player.cards[i] === player.cards[i+2])) {
        player.value = 4;
        player.tieBreaker = player.cards[i];
        player.combination = "Three of a Kind";
        return 
        }
      }
    }
      
  const pair = function (player) {
    let index = 0;
    for(let i = 0; i <= 3; i++) {
      if(player.cards[i] === player.cards[i+1]){
        index = i+2;
        player.value = 2;
        player.tieBreaker = player.cards[i];
        player.combination = "One Pair";
        break;
      }
    }

    if((index < 4) && (index > 0)) {
      for(let i = index; i <= 3; i++) {
        if(player.cards[i] === player.cards[i+1]){
          player.value = 3;
          player.tieBreaker = player.cards[i];
          player.combination = "Two Pairs";
          break;
        }

    }
  }}

  const sameCards = function (player) {
    pair(player);
    threeOfKind(player);
    fourOfKind(player);
    } 


  const init = function (player) {
    straight(player);
    flush(player);
    sameCards(player);
    fullHouse(player);
    // console.log(player);
  }

  const game = function (player1, player2) {
    init(player1);
    init(player2);
    if(player1.value !== player2.value) {
      player1.value > player2.value
        ? (player1Points++ && matchesPlayer1Won.push(i))
        : player2Points++
    } else {
      if(player1.tieBreaker !== player2.tieBreaker) {
        player1.tieBreaker > player2.tieBreaker
          ? (player1Points++ && matchesPlayer1Won.push(i))
          : player2Points++
      }else if(player1.highest !== player2.highest){
        player1.highest > player2.highest
          ? (player1Points++ && matchesPlayer1Won.push(i))
          : player2Points++
      }else {
        while(true) {
          let index = 4;
          player1.highest = player1.highest[index-1];
          player2.highest = player2.highest[index-1];
          if(player1.highest !== player2.highest){
            if(player1.highest > player2.highest){
              (player1Points++ && matchesPlayer1Won.push(i))
              break;
            } else {
              player2Points++;
              break;
            }
        }else {
          index --;
          if(index === 0){
            ties++;
            break;
          }
        }
      }
    }
  }}

  game(player1, player2);

}
console.log(`There were ${gamesCounter} games`);
console.log(`Player 1 won ${player1Points} times`);
console.log(`Player 2 won ${player2Points}`);
console.log(`Matches player1 won: ${matchesPlayer1Won}`);
console.log(`There were ${ties} ties`);


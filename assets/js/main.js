// Script minimal : ajouter interactions légères ici
document.addEventListener('DOMContentLoaded', function(){
  // Exemple: scroll smooth pour ancres
  document.querySelectorAll('a[href^="#"]').forEach(a=>{
    a.addEventListener('click', e=>{
      const target = document.querySelector(a.getAttribute('href'));
      if(target){
        e.preventDefault();
        target.scrollIntoView({behavior:'smooth'});
      }
    });
  });
});

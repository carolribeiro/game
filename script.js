(function($) {
		"use strict";
		
			/*	Carousel
			/*----------------------------------------------------*/
			$(".main-carousel").on("slide.bs.carousel", function() {
				$(".carousel-caption h1 span").removeClass('animated fadeInLeft');
				$(".carousel-caption h2 span").removeClass('animated fadeInRight');
				$(".carousel-caption p").removeClass('animated fadeInLeft');
			});
			$(".main-carousel").on("slid.bs.carousel", function() {
				$(this).find(".item.active .carousel-caption h1 span").addClass('animated fadeInLeft');
				$(this).find(".item.active .carousel-caption h2 span").addClass('animated fadeInRight');
				$(this).find(".item.active .carousel-caption p").addClass('animated fadeInLeft');
			});
			
			/*	Masonry
			/*----------------------------------------------------*/
			$('.masonry').imagesLoaded( function(){
				$('.masonry').masonry({itemSelector: '.elem', gutter: 27 }); 
			});
			
			/* Load Content
			/*----------------------------------------------------*/	
			$(".loaded-content section").slice(0, 4).show();
			$('#load-more').click(function (e) {
				e.preventDefault();
				var btn = $(this)
				btn.button('loading')
				setTimeout(function () {
					btn.button('reset')
					$(".loaded-content section:hidden").slice(0, 4).fadeIn();
				}, 500)
			});
			
			/*	Owl carousel
			/*----------------------------------------------------*/
			var owl = $(".owl-carousel");
			 
			owl.owlCarousel({
			items : 4, //4 items above 1000px browser width
			itemsDesktop : [1000,3], //3 items between 1000px and 0
			itemsTablet: [600,1], //1 items between 600 and 0
			itemsMobile : false // itemsMobile disabled - inherit from itemsTablet option
			});
			 
			// Custom Navigation Events
			$(".next").click(function(){
			owl.trigger('owl.next');
			return false;
			})
			$(".prev").click(function(){
			owl.trigger('owl.prev');
			return false;
			})
			$(".play").click(function(){
			owl.trigger('owl.play',1000); //owl.play event accept autoPlay speed as second parameter
			return false;
			})
			$(".stop").click(function(){
			owl.trigger('owl.stop');
			return false;
			})
			
			/*	Owl carousel
			/*----------------------------------------------------*/
			var owlv = $(".owl-video-carousel");
			 
			owlv.owlCarousel({
			items : 4, //4 items above 1000px browser width
			itemsDesktop : [1000,3], //3 items between 1000px and 0
			itemsTablet: [600,1], //2 items between 600 and 0
			itemsMobile : false // itemsMobile disabled - inherit from itemsTablet option
			});
			
			 /*	Owl carousel
			/*----------------------------------------------------*/
			$(".owl-widget-carousel").owlCarousel({
				autoPlay: true,
				singleItem:true
			});
		})(jQuery);
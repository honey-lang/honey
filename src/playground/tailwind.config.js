/** @type {import("tailwindcss").Config} */
module.exports = {
    content: ["./src/**/*.{html,js,jsx,ts,tsx}"],
    theme: {
      extend: {
        colors: {
          oxford: {
            50: "#CED9E3",
            100: "#BBDBEA",
            200: "#96B7D4",
            300: "#39668F",
            400: "#1C3148",
            500: "#1C3046",
            600: "#162538",
            700: "#101B29",
            800: "#0A101B",
            900: "#04050c",
          },
        },
        fontFamily: {
          "nunito-sans": "'Nunito Sans'",
        },
      },
    },
    plugins: [require("daisyui")],
    daisyui: {
        themes: [
            {
                honey: {
                  "primary": "#ffb238",
                  "secondary": "#ffe594",
                  "accent": "#ffffff",
                  "neutral": "#ffffff",
                  "base-100": "#04050c",
                },
            },
            "dark",
        ],
    },
  };
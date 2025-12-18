# KnoxStats 📊

**A Pedagogical R Package for Learning Statistics Through Practice**

![R](https://img.shields.io/badge/R-%23276DC3.svg?style=for-the-badg&logo=r&logoColor=white)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![Version](https://img.shields.io/badge/Version-0.1.0-blue.svg)
![Status](https://img.shields.io/badge/Status-Active-success.svg)

## 🎯 Overview

KnoxStats is an educational R package designed specifically for undergraduate statistics students learning R for the first time. The package provides intuitive, well-documented functions that make statistical concepts tangible through practical implementation.

> **Learning statistics should be about understanding concepts, not wrestling with code complexity.**

## ✨ Key Features

- **Beginner-Friendly Functions**: Clear parameter names and comprehensive error messages
- **Practical Examples**: Real-world datasets and scenarios students can relate to
- **Educational Documentation**: Each function explains the "why" behind the "how"

## 📦 Installation

You can install the development version from GitHub:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install KnoxStats
devtools::install_github("ojforsberg/KnoxStats")
```
 
## 🏫 Designed for the Classroom

### Features That Help Students Learn:

1. **Plain Language Outputs**: Statistical results explained in simple terms
2. **Step-by-Step Mode**: Optional detailed output showing calculations
3. **Common Mistakes Prevention**: Helpful error messages when students make common errors
4. **Learning Objectives**: Each function clearly states what concepts it teaches
5. **Practice Exercises**: Built-in exercises with solutions

### Example: Learning Mode

```r
# Enable learning mode for detailed output
options(knoxstats_learning_mode = TRUE)

# Now functions show their work
result <- simple_ttest(c(1,2,3,4,5), c(2,3,4,5,6))
# Output includes:
# - Step-by-step calculation
# - Formula used
# - Interpretation guide
# - Common misconceptions
```

## 🔧 For Instructors

### Customizing for Your Course

```r
# Set course-specific defaults
set_course_defaults(
  alpha = 0.05,
  decimal_places = 3,
  language = "simple"  # "simple", "technical", or "detailed"
)

# Create custom assignments
assignment <- create_assignment(
  topic = "Hypothesis Testing",
  difficulty = "intermediate",
  learning_objectives = c("t-tests", "p-values", "effect size")
)
```

### Integration with Learning Management Systems

KnoxStats outputs can be easily integrated with:
- R Markdown for reproducible reports
- Shiny apps for interactive learning
- Learning management system quizzes
- Jupyter notebooks with R kernel

## 📚 Documentation

- **Function Reference**: Complete documentation for all functions
- **Vignettes**: Detailed tutorials with examples
- **Cheat Sheets**: Quick reference guides for students
- **Video Tutorials**: Screen recordings of common analyses

View the full documentation at: [knoxstats.github.io](https://knoxstats.github.io)

## 🤝 Contributing

We welcome contributions from educators and students! Whether you're:
- **An instructor** with ideas for new teaching functions
- **A student** who found a confusing concept that needs better explanation
- **A developer** who wants to improve the codebase

Please see our [Contributing Guidelines](CONTRIBUTING.md) and [Code of Conduct](CODE_OF_CONDUCT.md).

### Quick Contribution Ideas:
1. Add an example dataset from your research area
2. Create a tutorial for a specific statistical concept
3. Improve error messages for common student mistakes
4. Add translations for non-English speaking students

## 📄 License

This package is released under the GPL-3 License. See the [LICENSE](LICENSE) file for details.

## 📞 Support & Community

- **GitHub Issues**: Report bugs or request features
- **Discussions**: Ask questions and share teaching ideas
- **Email**: ojforsberg@knox.edu

## 🙏 Acknowledgments

KnoxStats was developed with support from:
- Knox College of Illinois
- Feedback from hundreds of undergraduate students

---

**Made with ❤️ for statistics education**

*"The best way to learn statistics is to do statistics."*

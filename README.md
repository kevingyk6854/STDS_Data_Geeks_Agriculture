# STDS_Data_Geeks_Agriculture

## Repository Structure 

- [datasets]            ---> used to save datasets
  - raw_datasets
  - processed_datasets

- [notebooks]           ---> used to save and share ideas, information even questions.

- [project]             ---> used to save project (R)
  - build               ---> to build the project (not sure yet)
  - src                 ---> to put codes, template file or other files necessary to run the project

- [researches]          ---> used to save researches that members found 
  
  
## Basic Git Commands That You Might Need
- `git clone <remote_URL>`                      --> To create local copy of an existing remote repository, here you could do this 'git clone https://github.com/kevingyk6854/STDS_Data_Geeks_Agriculture.git'
- `git pull`                                    --> To get the latest version of a repository  
- `git status`                                  --> To check the current state of the local repository compared with remote one  
- `git add <file or directory name>`            --> To add a specific file or folder (including files) to the staging area for git  
- `git add .`                                   --> To add all file  
- `git commit -m <commit message in quotes>`    --> Record the changes made to files to a local repository (Note: please add a message within the command and let us know what you have done in this submission !)  
- `git checkout <branch_name>`                  --> To switch branch  
- `git rm -f <file_name>`                       --> To delete a file  
- `git reset <file or directory name>`                    --> To remove a file from the staging area
- `git push`                                    --> To send local commits to the remote   repository (Note: please make sure not submit files that would make the whole project breakdown !!!)

Note: If you need any other extra help related to git, you could check these websites:  
> https://git-scm.com/docs  
> http://guides.beanstalkapp.com/version-control/common-git-commands.html  

## How to upload large file (>100MB) to Github
1. download and install LFS  
- `brew install git-lfs`    --> Homebrew  
- `git lfs install`         --> set up LFS  
2. tell LFS which file you want to mark it as 'a large file'
- `git lfs track "<file_name>"`
3. add '.gitattributes' and commit it first
- `git add .gitattributes`
- `git commit -m "modify .gitattributes for lfs"` 
- `git push`
4. add large files and commit them
- `git add <files>`
- `git commit -m "<message>"`
- `git push`

Note: 
1. Highly recommand upload '.gitattributes' first, then upload large files. Otherwise, you might get trouble in 'git push' !!!!
2. If you get trouble with 'git commit' and want to withdraw that action, please type in 'git reset --soft HEAD~1' or 'git reset --soft HEAD^'. If you need more help you can check this website: https://git-scm.com/docs/git-reset
2. If you need any other extra help related to lfs, you could check these websites:  
> https://git-lfs.github.com/  
> https://www.jianshu.com/p/3f25cd20e392 (Chinese)  

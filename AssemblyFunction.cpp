//===-- AssemblyFunction.cpp ------------------------------------------*- C++
//-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//


#include "AssemblyFunction.h"
#include <bits/stdc++.h>
#include <stdlib.h>
#include <cxxabi.h>
#include <functional>
#include <macro.h>

using namespace std;


AssemblyFunction::AssemblyFunction(string function_name, uint64_t start_address, uint64_t end_address){

  FunctionPrototype = "";
  FunctionName  = function_name;
  startAddress  = start_address;
  endAddress    = end_address;
  ArgumentNum   = 0;
  ReturnWidth   = 0;
  ReturnType    = 0;

  Argument.clear();
  HasReturn = false;
	string argument = demangle(function_name);
  if(argument != "NULL"){
    ParseArgument(argument);
    ArgumentNum = Argument.size();
  }
  

  
}

// AssemblyFunction::AssemblyFunction(const AssemblyFunction& func) {
//   assert(&func);
//   printf("Warn: Function 发生了拷贝\n");
//   this->FunctionPrototype = func.FunctionPrototype;
//   this->FunctionName = func.FunctionName;
//   this->startAddress = func.startAddress;
//   this->endAddress = func.endAddress;
//   this->ArgumentNum = func.ArgumentNum;
//   this->HasReturn = func.HasReturn;
//   this->ReturnType = func.ReturnType;
  
//   for (auto arg : Argument) {
//     this->Argument.push_back(arg);
//   }

// }

//int AssemblyFunction::getSize() const { return this->Size; }

string AssemblyFunction::getName() const { return this->FunctionName; }

uint64_t AssemblyFunction::getStartAddress() const{ return startAddress; }

uint64_t AssemblyFunction::getEndAddress() const{ return endAddress; }

int AssemblyFunction::GetWidth(string str){

     if(str.find("char")!= string::npos)
        return 8;
     else if(str.find("short")!= string::npos)
        return 16;
     else if(str.find("int")!= string::npos || str.find("float")!= string::npos)
        return 32;
     else if(str.find("long" )!= string::npos || str.find("double")!= string::npos)
        return 64;
     // struct goes with 8
     else 
        return 8;

}


void AssemblyFunction::ParseArgument(string op){

    int width = 0;
    stringstream ss(op);
    string str;
    int sign = 0;
    while (ss.good()) {
        getline(ss, str, ',');
        if(str.find("unsigned") == string::npos)
            sign = 0b01;
        if(str.find('*') == string::npos)
            sign |= 0b10;
        Argument.push_back(make_pair(GetWidth(str),sign));
        sign = 0;
    }
}


string AssemblyFunction::demangle(string name){


    const char *mangled_name = name.c_str();
    int status = -1;
    char *demangled_name = abi::__cxa_demangle(mangled_name, NULL, NULL, &status);
    
	  //printf("Demangled: %s\n", demangled_name);
    if(status != 0)
       return "NULL";
    
    FunctionPrototype = demangled_name;
    name = demangled_name;
    int pos = name.find("(");
    int len = name.length();
    if(pos!=string::npos)
      name = name.substr(pos+1, len-pos-2);


    //cout << "Argument: " << name <<endl;
    //free(demangled_name);
    return name;


}


void AssemblyFunction::dump(){

    cout << "Function Dump!\n\tFunction Name: " << getName() << endl;
    cout << "\tFunction Range from " << getStartAddress() << " to " << getEndAddress() << endl;

    cout << "\tFunction Prototype: " << getPrototype()<<endl;
    cout << "\t# of Argument: " << getArgumentNum() << endl;

    for(auto it = Argument.begin();it!= Argument.end();it++){
        cout << "\tOperand width = "<<it->first;
        if(it->second & 0b01)
            cout << ", Signed";
        else
            cout << ", Unsigned";
        if(it->second & 0b10)
            cout << ", Data\n";
        else
            cout << ", IsPointer\n";
    }
    if(HasReturn){
      cout << "\tReturn Type = ";
      if(ReturnType == IsAddr)
        cout << "Pointer;";
      else if(ReturnType == IsData)
        cout << "Data;";
      else 
        cout << "Invalid Return Type!!!";
        
      cout << "\tReturn Width = " << ReturnWidth <<endl;


    }

}

int AssemblyFunction::getArgumentNum(){
  return ArgumentNum;
}

string AssemblyFunction::getPrototype(){
  return FunctionPrototype;
}

bool AssemblyFunction::hasReturn(){
  return HasReturn;
}

void  AssemblyFunction::setReturn(int width){
  HasReturn = true;
  ReturnWidth = width;
}

void AssemblyFunction::setReturnType(int PtrOrData){

  ReturnType = PtrOrData;
}


void AssemblyFunction::setReturnWidth(int width){
   this->ReturnWidth = width;
}

int AssemblyFunction::getReturnWidth(){
  return this->ReturnWidth;
}

int AssemblyFunction::getReturnType(){
  return this->ReturnType;
}


uint64_t AssemblyFunction::hashCode() {
  return startAddress ^ (endAddress << 1);
}

AssemblyFunction::~AssemblyFunction() {}
